// Используется метод рекурсивного спуска: каждая грамматическая конструкция - отдельная функция.

//  Грамматика Poof (упрощённо):
//
//    program    ::= statement* EOF
//    statement  ::= bind | spell | whisper | expr
//    bind       ::= "bind" IDENT "to" expr
//    spell      ::= "spell" IDENT IDENT+ "->" expr
//    whisper    ::= "whisper" expr
//    expr       ::= letIn | ifExpr | whenExpr | lambda
//                 | invoke | cast | comparison
//    lambda     ::= IDENT "->" expr
//    invoke     ::= "invoke" expr "with" expr "on" expr
//    cast       ::= "cast" IDENT "with" expr
//    ifExpr     ::= "if" expr "then" expr "else" expr
//    whenExpr   ::= "when" expr NEWLINE (pattern "->" expr NEWLINE)+
//    comparison ::= addition (("==" | "!=" | "<" | ">" | "<=" | ">=") addition)*
//    addition   ::= multiply (("+" | "-" | "++") multiply)*
//    multiply   ::= unary (("*" | "/" | "%") unary)*
//    unary      ::= "!" unary | apply
//    apply      ::= atom atom*           -- применение функции: f x y
//    atom       ::= INT | FLOAT | STRING | BOOL | IDENT
//                 | "(" expr ")" | "[" exprList "]"
//    pattern    ::= INT | STRING | "true" | "false" | "_" | IDENT

module Poof.Parser

open Poof.AST
open Poof.Lexer

// Ошибки парсера
exception ParserError of message: string * token: Token

let parseError msg tok = 
    raise (ParserError(msg, tok))

// Состояние парсера
type ParserState = {
    Tokens : Token array // весь список токенов от лексера
    mutable Pos : int // текущая позиция
}

let createParser tokens = 
    { Tokens = Array.ofList tokens; Pos = 0 }

// Текущий токен без продвижения
let current (s: ParserState) =
    if s.Pos < s.Tokens.Length then s.Tokens.[s.Pos]
    else EOF

// Посмотреть вперед на N позиций
let peekAt (s: ParserState) offset =
    let i = s.Pos + offset
    if i < s.Tokens.Length then s.Tokens.[i]
    else EOF

// Продвинуться вперед на токен
let advance (s: ParserState) =
    let tok = current s
    s.Pos <- s.Pos + 1
    tok

let skipNewlines (s: ParserState) = 
    while current s = NEWLINE do
        s.Pos <- s.Pos + 1

let expect (s: ParserState) tok =
    let actual = advance s
    if actual <> tok then
        parseError $"Ожидается {tokenToString tok}, вместо {tokenToString actual}" actual
    
let expectIdent (s: ParserState) = 
    match advance s with
    | IDENT name -> name
    | tok -> parseError $"Ожидается идентификатор, вместо {tokenToString tok}" tok

// Проверить текущий токен без продвижения
let check (s: ParserState) tok = current s = tok

let tryConsume (s: ParserState) tok = 
    if current s = tok then
        s.Pos <- s.Pos + 1
        true
    else
        false

// Парсер выражений

// Точка входа
let rec parseExpr (s: ParserState) : Expr = 
    skipNewlines s
    match current s with
    | IDENT _ when peekAt s 1 = ARROW ->
        let param = expectIdent s
        expect s ARROW
        let body = parseExpr s
        Lambda(param, body)
    | IF ->
        parseIf s
    | WHEN ->
        parseWhen s
    | INVOKE ->
        parseInvoke s
    | CAST ->
        parseCast s
    | _ ->
        parseOr s

and parseIf (s: ParserState) : Expr =
    expect s IF
    skipNewlines s
    let cond = parseExpr s
    skipNewlines s
    expect s THEN
    skipNewlines s
    let thenBranch = parseExpr s 
    skipNewlines s
    expect s ELSE
    skipNewlines s
    let elseBranch = parseExpr s
    If(cond, thenBranch, elseBranch)

and parseWhen (s: ParserState) : Expr =
    expect s WHEN
    let subject = parseExpr s
    skipNewlines s
    let cases = System.Collections.Generic.List<Pattern * Expr>()
    let rec readCases () = 
        skipNewlines s 
        match current s with
        | INT _ | STRING _ | TRUE | FALSE | UNDERSCORE | IDENT _ -> 
            match current s with
            | IDENT name when isTopLevel name ->
                ()
            | _ ->
                let pat = parsePattern s 
                skipNewlines s
                expect s ARROW
                skipNewlines s
                let body = parseExpr s
                cases.Add((pat, body))
                readCases ()
        | _ -> ()
    readCases ()
    if cases.Count = 0 then
        parseError "Блок when должен содержать хотя бы одну ветку" (current s)
    When(subject, Seq.toList cases)

and isTopLevel = function
    | "bind" | "spell" | "whisper" | "invoke" | "cast" -> true
    | _ -> false

and parseInvoke (s: ParserState) : Expr = 
    expect s INVOKE
    skipNewlines s
    let func = parseAtom s
    skipNewlines s
    expect s WITH
    skipNewlines s
    let arg = parseExpr s
    skipNewlines s
    expect s ON
    skipNewlines s
    let target = parseExpr s
    Invoke(func, arg, target)

and parseCast (s: ParserState) : Expr =
    expect s CAST
    skipNewlines s
    let name = expectIdent s
    skipNewlines s
    // `cast f` возвращает значение функции; полная форма: `cast f with arg`
    if tryConsume s WITH then
        skipNewlines s
        let arg = parseExpr s
        Cast(name, arg)
    else
        Var name

and parseOr (s: ParserState) : Expr =
    let mutable left = parseAnd s
    while check s PIPEPIPE do
        advance s |> ignore
        let right = parseAnd s
        left <- BinOp("||", left, right)
    left

and parseAnd (s: ParserState) : Expr = 
    let mutable left = parseComparison s
    while check s AMPAMP do
        advance s |> ignore
        let right = parseComparison s
        left <- BinOp("&&", left, right)
    left

and parseComparison (s: ParserState) : Expr = 
    let left = parseAddition s
    match current s with
    | EQEQ | NEQ | LT | GT | LTE | GTE ->
        let op = advance s
        let right = parseAddition s
        let opStr =
            match op with
            | EQEQ -> "==" | NEQ -> "!=" | LT -> "<"
            | GT -> ">" | LTE -> "<=" | GTE -> ">="
            | _ -> failwith "unreachable"
        BinOp(opStr, left, right)
    | _ -> left

and parseAddition (s: ParserState) : Expr = 
    let mutable left = parseMultiply s
    let rec loop () = 
        match current s with
        | PLUS | MINUS | PLUSPLUS ->
            let op = advance s
            let right = parseMultiply s
            let opStr = 
                match op with
                | PLUS -> "+" | MINUS -> "-" | PLUSPLUS -> "++"
                | _ -> failwith "unreachable"
            left <- BinOp(opStr, left, right)
            loop ()
        | _ -> ()
    loop ()
    left

and parseMultiply (s: ParserState) : Expr = 
    let mutable left = parseUnary s
    let rec loop () =
        match current s with
        | STAR | SLASH | PERCENT ->
            let op = advance s
            let right = parseUnary s
            let opStr = 
                match op with
                | STAR -> "*" | SLASH -> "/" | PERCENT -> "%"
                | _ -> failwith "unreachable"
            left <- BinOp(opStr, left, right)
            loop ()
        | _ -> ()
    loop ()
    left

and parseUnary (s: ParserState) : Expr =
    match current s with
    | BANG ->
        advance s |> ignore 
        let operand = parseUnary s
        Apply(Var "not", operand)
    | _ ->
        parseApply s

and parseApply (s: ParserState) : Expr = 
    let func = parseAtom s
    let rec loop acc =
        match current s with
        | tok when isAtomStart tok ->
            let arg = parseAtom s
            loop (Apply(acc, arg))
        | _ -> acc
    loop func

and isAtomStart = function
    | INT _ | FLOAT _ | STRING _ | TRUE | FALSE
    | IDENT _ | LPAREN | LBRACKET -> true
    | _ -> false

and parseAtom (s: ParserState) : Expr = 
    match current s with
    | INT n ->
        advance s |> ignore 
        IntLit n
    | FLOAT f ->
        advance s |> ignore
        FloatLit f
    | STRING str ->
        advance s |> ignore
        StringLit str
    | TRUE -> advance s |> ignore; BoolLit true
    | FALSE -> advance s |> ignore; BoolLit false
    | IDENT name ->
        advance s |> ignore
        Var name
    
    | LPAREN ->
        advance s |> ignore 
        skipNewlines s
        if check s RPAREN then
            advance s |> ignore 
            Unit
        else 
            let expr = parseExpr s
            skipNewlines s
            expect s RPAREN
            expr
    | LBRACKET ->
        advance s |> ignore
        skipNewlines s
        if check s RBRACKET then
            advance s |> ignore
            ListLit []
        else
            let items = parseExprList s
            skipNewlines s
            expect s RBRACKET
            ListLit items
    | tok ->
        parseError $"Неожиданный токен в выражении: {tokenToString tok}" tok

and parseExprList (s: ParserState) : Expr list = 
    let items = System.Collections.Generic.List<Expr>()
    items.Add(parseExpr s)
    while tryConsume s COMMA do
        skipNewlines s
        items.Add(parseExpr s)
    Seq.toList items

and parsePattern (s: ParserState) : Pattern =
    match current s with
    | INT n ->
        advance s |> ignore
        PInt n
    | STRING str ->
        advance s |> ignore
        PString str
    | TRUE ->
        advance s |> ignore
        PBool true
    | FALSE ->
        advance s |> ignore
        PBool false
    | UNDERSCORE ->
        advance s |> ignore
        PWild
    | IDENT name ->
        advance s |> ignore
        PVar name
    | tok ->
        parseError $"Ожидался паттерн (число, строка, _. или имя), но встретился: {tokenToString tok}" tok

// Парсер statements (верхний уровень)
and parseStatement (s: ParserState) : Expr = 
    skipNewlines s
    match current s with
    | BIND ->
        advance s |> ignore
        let name = expectIdent s
        expect s TO
        let value = parseExpr s
        skipNewlines s
        let body = 
            if current s = EOF then Unit
            else parseStatement s
        Bind(name, value, body)

    | SPELL ->
        advance s |> ignore
        let name = expectIdent s
        let mutable parms = []
        while current s <> ARROW && current s <> EOF do
            match current s with
            | IDENT p -> advance s |> ignore; parms <- parms @ [p]
            | tok -> parseError $"Ожидался параметр функции, встретилось: {tokenToString tok}" tok
        if parms.IsEmpty then
            parseError $"Функция {name} должна иметь хотя бы один параметр" (current s)
        expect s ARROW
        skipNewlines s
        let body = parseExpr s
        skipNewlines s
        let lambda =
            List.foldBack (fun param acc -> Lambda(param, acc)) parms body
        let rest =
            if current s = EOF then Unit 
            else parseStatement s
        Spell(name, List.head parms, lambda)
        |> fun _ ->
            Bind(name, lambda, rest)

    | WHISPER ->
        advance s |> ignore
        let expr = parseExpr s
        skipNewlines s
        let rest = 
            if current s = EOF then Unit
            else parseStatement s
        Sequence[Whisper expr; rest]
    
    | _ ->
        let expr = parseExpr s
        skipNewlines s
        if current s = EOF then expr
        else
            let rest = parseStatement s
            Sequence[expr; rest]

// Парсинг всей программы
let parse (tokens: Token list) : Expr =
    let state = createParser tokens
    skipNewlines state
    if current state = EOF then
        Unit
    else 
        let ast = parseStatement state
        skipNewlines state
        match current state with
        | EOF -> ast
        | tok -> parseError $"Неожиданный токен в конце программы: {tokenToString tok}" tok

let parseString (source: string) : Expr =
    source
    |> Lexer.tokenize
    |> parse

// Pretty-printer для ast (для отладки)
let rec prettyPrint (indent: int) (expr: Expr) : unit =
    let pad = String.replicate indent " "
    match expr with
    | IntLit n -> printfn $"{pad}IntLit {n}"
    | FloatLit f -> printfn $"{pad}FloatLit {f}"
    | BoolLit b -> printfn $"{pad}BoolLit {b}"
    | StringLit s -> printfn $"{pad}StringLit \"{s}\""
    | Unit -> printfn $"{pad}Unit"
    | Var name -> printfn $"{pad}Var \"{name}\""

    | Bind(name, value, body) ->
        printfn $"{pad}Bind \"{name}\""
        prettyPrint (indent + 1) value
        printfn $"{pad} in:"
        prettyPrint (indent + 1) body
    
    | Spell(name, param, body) ->
        printfn $"{pad}Spell \"{name}\" param=\"{param}\""
        prettyPrint (indent + 1) body

    | Lambda(param, body) ->
        printfn $"{pad}Lambda \"{param}\""
        prettyPrint (indent + 1) body
    
    | Apply(func, arg) ->
        printfn $"{pad}Apply"
        prettyPrint (indent + 1) func
        prettyPrint (indent + 1) arg

    | Cast(name, arg) ->
        printfn $"{pad}Cast \"{name}\""
        prettyPrint (indent + 1) arg

    | If(cond, thenB, elseB) ->
        printfn $"{pad}If"
        printfn $"{pad} cond:"
        prettyPrint (indent + 2) cond
        printfn $"{pad} then:"
        prettyPrint (indent + 2) thenB
        printfn $"{pad} else:"
        prettyPrint (indent + 2) elseB

    | When(subject, cases) ->
        printfn $"{pad}When"
        prettyPrint (indent + 1) subject
        cases |> List.iter (fun (pat, body) ->
            printfn $"{pad} | {pat} ->"
            prettyPrint (indent + 2)  body)

    | Invoke(func, arg, target) ->
        printfn $"{pad}Invoke"
        prettyPrint (indent + 1) func
        printfn $"{pad} with:"
        prettyPrint (indent + 1) arg
        printfn $"{pad} on:"
        prettyPrint (indent + 1) target

    | Whisper expr ->
        printfn $"{pad}Whisper"
        prettyPrint (indent + 1) expr

    | ListLit items ->
        printfn $"{pad}ListLit [{items.Length} items]"
        items |> List.iter (prettyPrint (indent + 1))

    | BinOp(op, left, right) ->
        printfn $"{pad}BinOp \"{op}\""
        prettyPrint (indent + 1) left
        prettyPrint (indent + 1) right

    | Sequence expr ->
        printfn $"{pad}Sequence"
        expr |> List.iter (prettyPrint (indent + 1))

    | UnariMinus expr ->
        printfn $"{pad}UnariMinus"
        prettyPrint (indent + 1) expr

let debugParse source = 
    printfn "---AST Poof---"
    try
        let ast = parseString source
        prettyPrint 0 ast
        printfn "--------------"
        Some ast
    with
    | ParserError(msg, tok) ->
        printfn $"Ошибка парсера: {msg}"
        printfn $" На токене: {tokenToString tok}"
        printfn "--------------"
        None
    | Lexer.LexerError(msg, line, col) ->
        printfn $"Ошибка лексера в строке {line}, колонке {col}: {msg}"
        printfn "--------------"
        None
    