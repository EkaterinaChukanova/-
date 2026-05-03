module Poof.Interpreter

open Poof.AST
open Poof.Parser
open Poof.Enviroment

type private Trampoline<'a> =
    | Done of 'a
    | Step of (unit -> Trampoline<'a>)

let rec private runTramp (t: Trampoline<'a>) : 'a =
    match t with
    | Done x -> x
    | Step k -> runTramp (k ())

let private bindT (m: Trampoline<'a>) (f: 'a -> Trampoline<'b>) : Trampoline<'b> =
    let rec run m =
        match m with
        | Done x -> f x
        | Step k -> Step (fun () -> run (k ()))
    run m

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

let rec private applyTramp (func: Value) (arg: Value) : Trampoline<Value> =
    match func with
    | VFunc(param, body, closure) ->
        let env' = bindValue param arg closure
        Step (fun () -> evalTramp env' body)
    | VBuiltin(_, fn) -> Done (fn arg)
    | _ -> Done (runtimeError $"'{valueToString func}' is not a spell — it cannot be cast")

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

and private evalTramp (env: Env) (expr: Expr) : Trampoline<Value> =
    match expr with
    | IntLit n -> Done (VInt n)
    | FloatLit f -> Done (VFloat f)
    | BoolLit b -> Done (VBool b)
    | StringLit s -> Done (VString s)
    | Unit -> Done VUnit

    | ListLit xs ->
        let rec foldList acc rest =
            match rest with
            | [] -> Done (VList (List.rev acc))
            | e :: es ->
                bindT (evalTramp env e) (fun v -> foldList (v :: acc) es)
        foldList [] xs

    | Var name -> Done (lookupOrFail name env)

    | UnariMinus e ->
        bindT (evalTramp env e) (fun v ->
            Done(
                match v with
                | VInt n -> VInt (-n)
                | VFloat f -> VFloat (-f)
                | _ -> runtimeError $"Cannot negate '{valueToString v}'"
            ))

    | BinOp(op, left, right) ->
        bindT (evalTramp env left) (fun l ->
            bindT (evalTramp env right) (fun r ->
                Done(
                    match op with
                    | "==" -> VBool (l = r)
                    | "!=" -> VBool (l <> r)
                    | "&&" -> VBool (requireBool l && requireBool r)
                    | "||" -> VBool (requireBool l || requireBool r)
                    | "++" -> VString (requireString l + requireString r)
                    | "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" -> numBinOp op l r
                    | ";" -> r
                    | _ -> runtimeError $"Unknown operator: {op}"
                )))

    | If(cond, tBranch, fBranch) ->
        bindT (evalTramp env cond) (fun c ->
            if requireBool c then evalTramp env tBranch else evalTramp env fBranch)

    | Lambda(param, body) ->
        Done (VFunc(param, body, env))

    | Apply(funcE, argE) ->
        bindT (evalTramp env funcE) (fun fv ->
            bindT (evalTramp env argE) (fun av -> applyTramp fv av))

    | Cast(name, arg) ->
        let f = lookupOrFail name env
        bindT (evalTramp env arg) (fun av -> applyTramp f av)

    | Invoke(fExpr, argExpr, targetExpr) ->
        bindT (evalTramp env fExpr) (fun f ->
            bindT (evalTramp env argExpr) (fun a ->
                bindT (applyTramp f a) (fun fa ->
                    bindT (evalTramp env targetExpr) (fun t -> applyTramp fa t))))

    | Bind(name, valueExpr, body) ->
        let cell = ref VUnit
        let envWithRec = { env with Bindings = env.Bindings |> Map.add name (VRef cell) }
        bindT (evalTramp envWithRec valueExpr) (fun v ->
            cell := v
            evalTramp envWithRec body)

    | Whisper e ->
        bindT (evalTramp env e) (fun v ->
            printfn "%s" (valueToString v)
            Done VUnit)

    | Sequence exprs ->
        let rec walk last = function
            | [] -> Done last
            | e :: es ->
                bindT (evalTramp env e) (fun v -> walk v es)
        walk VUnit exprs

    | When(subject, cases) ->
        bindT (evalTramp env subject) (fun v ->
            let rec go cs =
                match cs with
                | [] -> Done (runtimeError "No matching pattern found in 'when'")
                | (pat, body) :: rest ->
                    match matchPattern pat v with
                    | Some bindings ->
                        let env' =
                            bindings |> List.fold (fun e (n, value) -> bindValue n value e) env
                        evalTramp env' body
                    | None -> go rest
            go cases)

    | Spell(name, _, _) ->
        Done (runtimeError $"Unexpected Spell node for '{name}' — should have been desugared by parser")

let applyValue (func: Value) (arg: Value) : Value =
    runTramp (applyTramp func arg)

let eval (env: Env) (expr: Expr) : Value =
    runTramp (evalTramp env expr)

let run (source: string) : Value =
    let ast = parseString source
    let env = initGlobalEnv applyValue
    eval env ast

let runFile (path: string) : Value =
    let source = System.IO.File.ReadAllText path
    run source
