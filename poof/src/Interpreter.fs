module Poof.Interpreter

open Poof.AST
open Poof.Parser
open Poof.Enviroment

let private requireInt = function
    | VInt n -> n
    | v -> runtimeError $"Expected Int, but got {valueToString v}"

let private requireFloat = function
    | VFloat f -> f
    | VInt n -> float n
    | v -> runtimeError $"Expected Float, but got {valueToString v}"

let private requireBool = function
    | VBool b -> b
    | v -> runtimeError $"Expected Bool, but got {valueToString v}"

let private requireString = function
    | VString s -> s
    | v -> runtimeError $"Expected String, but got {valueToString v}"

let private requireList = function
    | VList xs -> xs
    | v -> runtimeError $"Expected List, but got {valueToString v}"

let private numBinOp (op: string) (l: Value) (r: Value) : Value =
    match l, r with
    | VInt a, VInt b ->
        match op with
        | "+" -> VInt (a + b)
        | "-" -> VInt (a - b)
        | "*" -> VInt (a * b)
        | "/" ->
            if b = 0 then runtimeError "Division by zero"
            VInt (a / b)
        | "%" ->
            if b = 0 then runtimeError "Division by zero"
            VInt (a % b)
        | "<" -> VBool (a < b)
        | ">" -> VBool (a > b)
        | "<=" -> VBool (a <= b)
        | ">=" -> VBool (a >= b)
        | _ -> runtimeError $"Unknown numeric operator: {op}"
    | _ ->
        let a = requireFloat l
        let b = requireFloat r
        match op with
        | "+" -> VFloat (a + b)
        | "-" -> VFloat (a - b)
        | "*" -> VFloat (a * b)
        | "/" ->
            if b = 0.0 then runtimeError "Division by zero"
            VFloat (a / b)
        | "<" -> VBool (a < b)
        | ">" -> VBool (a > b)
        | "<=" -> VBool (a <= b)
        | ">=" -> VBool (a >= b)
        | _ -> runtimeError $"Unknown numeric operator: {op}"

let rec private applyValue (func: Value) (arg: Value) : Value =
    match func with
    | VFunc(param, body, closure) ->
        let env' = bindValue param arg closure
        eval env' body
    | VBuiltin(_, fn) -> fn arg
    | _ -> runtimeError $"'{valueToString func}' is not a spell — it cannot be cast"

and private matchPattern (pat: Pattern) (v: Value) : (string * Value) list option =
    match pat, v with
    | PWild, _ -> Some []
    | PVar name, _ -> Some [ (name, v) ]
    | PInt a, VInt b when a = b -> Some []
    | PFloat a, VFloat b when a = b -> Some []
    | PBool a, VBool b when a = b -> Some []
    | PString a, VString b when a = b -> Some []
    | PList ps, VList vs when ps.Length = vs.Length ->
        let folder (acc: (string * Value) list option) (p, x) =
            match acc, matchPattern p x with
            | Some a, Some b -> Some (a @ b)
            | _ -> None
        List.zip ps vs |> List.fold folder (Some [])
    | _ -> None

and eval (env: Env) (expr: Expr) : Value =
    match expr with
    | IntLit n -> VInt n
    | FloatLit f -> VFloat f
    | BoolLit b -> VBool b
    | StringLit s -> VString s
    | Unit -> VUnit
    | ListLit xs -> VList (xs |> List.map (eval env))
    | Var name -> lookupOrFail name env

    | UnariMinus e ->
        match eval env e with
        | VInt n -> VInt (-n)
        | VFloat f -> VFloat (-f)
        | v -> runtimeError $"Cannot negate '{valueToString v}'"

    | BinOp(op, left, right) ->
        let l = eval env left
        let r = eval env right
        match op with
        | "==" -> VBool (l = r)
        | "!=" -> VBool (l <> r)
        | "&&" -> VBool (requireBool l && requireBool r)
        | "||" -> VBool (requireBool l || requireBool r)
        | "++" -> VString (requireString l + requireString r)
        | "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" -> numBinOp op l r
        | ";" -> r
        | _ -> runtimeError $"Unknown operator: {op}"

    | If(cond, tBranch, fBranch) ->
        if requireBool (eval env cond) then eval env tBranch else eval env fBranch

    | Lambda(param, body) ->
        VFunc(param, body, env)

    | Apply(func, arg) ->
        let f = eval env func
        let a = eval env arg
        applyValue f a

    | Cast(name, arg) ->
        let f = lookupOrFail name env
        applyValue f (eval env arg)

    | Invoke(fExpr, argExpr, targetExpr) ->
        let f = eval env fExpr
        let a = eval env argExpr
        let t = eval env targetExpr
        applyValue (applyValue f a) t

    | Bind(name, valueExpr, body) ->
        // Рекурсивная привязка: имя видно внутри valueExpr (важно для spell fact -> ... cast fact ...)
        let cell = ref VUnit
        let envWithRec = { env with Bindings = env.Bindings |> Map.add name (VRef cell) }
        let v = eval envWithRec valueExpr
        cell := v
        eval envWithRec body

    | Whisper e ->
        let v = eval env e
        printfn "%s" (valueToString v)
        VUnit

    | Sequence exprs ->
        exprs |> List.fold (fun _ e -> eval env e) VUnit

    | When(subject, cases) ->
        let v = eval env subject
        let rec go = function
            | [] -> runtimeError "No matching pattern found in 'when'"
            | (pat, body) :: rest ->
                match matchPattern pat v with
                | Some bindings ->
                    let env' =
                        bindings
                        |> List.fold (fun e (n, value) -> bindValue n value e) env
                    eval env' body
                | None -> go rest
        go cases

    | Spell(name, _, _) ->
        runtimeError $"Unexpected Spell node for '{name}' — should have been desugared by parser"

let run (source: string) : Value =
    let ast = parseString source
    let env = initGlobalEnv applyValue
    eval env ast

let runFile (path: string) : Value =
    let source = System.IO.File.ReadAllText path
    run source

