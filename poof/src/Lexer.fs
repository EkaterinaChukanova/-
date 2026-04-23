module Poof.Lexer

type Token = 
    // Ключевые слова-заклинания
    | BIND // объявление переменной
    | TO // часть "bind x to expr"
    | SPELL // объявление именованной функции
    | CAST // рекурсивный вызов функции
    | WITH // часть "cast f with arg"
    | INVOKE // вызов функции высшего порядка
    | ON // часть "invoke f with g on list"
    | WHISPER // вывод на экран

    // Управляющие конструкции
    | IF
    | THEN
    | ELSE
    | WHEN // начало паттерн-матчиинга
    | IS // сравнение в условии

    // Булевы литералы
    | TRUE
    | FALSE

    // Литералы со значениями
    | INT of int
    | FLOAT of float
    | STRING of string
    | IDENT of string // идентификатор

    // Операторы
    | ARROW // "->" стрелка в spell/lambda/when
    | PLUS // "+"
    | MINUS // "-"
    | STAR // "*"
    | SLASH // "/"
    | PERCENT // "%"
    | EQEQ // "=="
    | NEQ // "!="
    | LT // "<"
    | GT // ">"
    | LTE // "<="
    | GTE // ">="
    | PLUSPLUS // "++" конкатенация строк
    | AMPAMP // "&&"
    | PIPEPIPE // "||"
    | BANG // "!"

    // Пунктуация
    | LPAREN // "("
    | RPAREN // ")"
    | LBRACKET // "["
    | RBRACKET // "]"
    | COMMA // ","
    | UNDERSCORE // "_" wildcard в паттерн-матчинге

    // Служебные токены
    | NEWLINE // конец строки
    | EOF // конец файла

// Tестовая печать для проверки работы лексера
let tokenToString = function
    | BIND -> "BIND"       | TO -> "TO"         | SPELL -> "SPELL"
    | CAST -> "CAST"       | WITH -> "WITH"      | INVOKE -> "INVOKE"
    | ON -> "ON"           | WHISPER -> "WHISPER"
    | IF -> "IF"           | THEN -> "THEN"      | ELSE -> "ELSE"
    | WHEN -> "WHEN"       | IS -> "IS"
    | TRUE -> "TRUE"       | FALSE -> "FALSE"
    | INT n    -> $"INT({n})"
    | FLOAT f  -> $"FLOAT({f})"
    | STRING s -> $"STRING(\"{s}\")"
    | IDENT s  -> $"IDENT({s})"
    | ARROW -> "->"        | PLUS -> "+"         | MINUS -> "-"
    | STAR -> "*"          | SLASH -> "/"        | PERCENT -> "%"
    | EQEQ -> "=="         | NEQ -> "!="         | LT -> "<"
    | GT -> ">"            | LTE -> "<="         | GTE -> ">="
    | PLUSPLUS -> "++"     | AMPAMP -> "&&"      | PIPEPIPE -> "||"
    | BANG -> "!"
    | LPAREN -> "("        | RPAREN -> ")"
    | LBRACKET -> "["      | RBRACKET -> "]"
    | COMMA -> ","         | UNDERSCORE -> "_"
    | NEWLINE -> "\\n"     | EOF -> "EOF"


// Ошибки лексера. Свое исключение с номером строки и позиции
exception LexerError of mesage: string * line: int * col: int

// Вспомогательная функция для бросания ошибки
let lexError msg line col = 
    raise (LexerError(msg, line, col))

// Состояние лексера. Храним все состояние в одной записи
type LexerState = {
    Source : string // исходный код целиком
    mutable Pos : int // текущая позиция в строке
    mutable Line : int // текущий номер строки
    mutable Col : int // текущая колонка
}

let createState source = { Source = source; Pos = 0; Line = 1; Col = 1 }

// Текущий символ без продвижения вперед
let current (s: LexerState) =
    if s.Pos < s.Source.Length then Some s.Source.[s.Pos]
    else None

// Заглянуть на один символ вперед
let peak (s: LexerState) =
    if s.Pos + 1 < s.Source.Length then Some s.Source.[s.Pos + 1]
    else None

// Продвинутся на один символ вперед
let advance (s: LexerState) =
    match current s with
    | Some '\n' ->
        s.Pos <- s.Pos + 1
        s.Line <- s.Line + 1
        s.Col <- 1
    | Some _ ->
        s.Pos <- s.Pos + 1
        s.Col <- s.Col + 1
    | None -> () 

// Вспомогательные предикаты

let isDigit c = c >= '0' && c <= '9'
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isAlphaNum c = isAlpha c || isDigit c || c = '_'
let isWhitespace c = c = ' ' || c = '\t' || c = '\r'

// Таблица ключевых слов
let keywords = 
    dict [
        "bind", BIND
        "to", TO
        "spell", SPELL
        "cast", CAST
        "with", WITH
        "invoke", INVOKE
        "on", ON
        "whisper", WHISPER
        "if", IF
        "then", THEN
        "else", ELSE
        "when", WHEN
        "is", IS
        "true", TRUE
        "false", FALSE
    ]

// Чтение отдельных видов токенов

// Комментарии: ~~ текст ~~
let skipComment (s: LexerState) =
    match current s with
    | Some '~' ->
        advance s
        let rec loop() =
            match current s with
            | None | Some '\n' -> ()
            | Some '~' ->
                advance s
                match current s with
                | Some '~' -> advance s
                | _ -> loop ()
            | _ -> advance s; loop ()
        loop ()
    | _ -> ()

// Строки
let readString (s: LexerState) =
    advance s
    let buf = System.Text.StringBuilder()
    let rec loop() =
        match current s with
        | None ->
            lexError "Незакрытая строка - ожидется '\"'" s.Line s.Col
        | Some '"' ->
            advance s
        | Some '\\' ->
            advance s
            match current s with
            | Some 'n' -> buf.Append('\n') |> ignore; advance s; loop ()
            | Some 't' -> buf.Append('\t') |> ignore; advance s; loop ()
            | Some '\\' -> buf.Append('\\') |> ignore; advance s; loop ()
            | Some '"' -> buf.Append('"') |> ignore; advance s; loop ()
            | Some c ->
                lexError $"Неизвестная escape-последовательность: \\{c}" s.Line s.Col
            | None ->
                lexError "Незакрытая строка после \\" s.Line s.Col
        | Some c ->
            buf.Append(c) |> ignore
            advance s
            loop ()
    loop ()
    STRING (buf.ToString())    

// Числа
let readNumber (s: LexerState) =
    let startPos = s.Pos
    while (match current s with Some c when isDigit c -> true | _ -> false) do
        advance s
    match current s, peak s with
    | Some '.', Some c when isDigit c ->
        advance s
        while (match current s with Some c when isDigit c -> true | _ -> false) do
            advance s
        let text = s.Source.[startPos .. s.Pos - 1]
        FLOAT (float text)
    | _ ->
        let text = s.Source.[startPos .. s.Pos - 1]
        INT (int text)

// Идентификаторы и ключевые слова
let readIdent (s: LexerState) =
    let startPos = s.Pos
    while (match current s with Some c when isAlphaNum c -> true | _ -> false) do
        advance s
    let word = s.Source.[startPos .. s.Pos - 1]
    match keywords.TryGetValue(word) with
    | true, kw -> kw
    | _ -> IDENT word

// Главная функция лексера
let rec nextToken (s: LexerState) : Token =
    while (match current s with Some c when isWhitespace c -> true | _ -> false) do
        advance s
    let line = s.Line
    let col = s.Col

    match current s with
    | None -> EOF
    | Some '\n' ->
        advance s
        while (match current s with Some '\n' -> true | _ -> false) do
            advance s
        NEWLINE 

    | Some '~' ->
        advance s
        skipComment s
        nextToken s 
    | Some '"' ->
        readString s 
    | Some c when isDigit c ->
        readNumber s
    | Some c when isAlpha c || c = '_' ->
        if c = '_' then 
            match peak s with 
            | Some next when isAlphaNum next ->
                readIdent s 
            | _ ->
                advance s 
                UNDERSCORE
        else
            readIdent s 

    | Some '-' ->
        advance s 
        match current s with
        | Some '>' -> advance s; ARROW
        | _ -> MINUS

    | Some '+' ->
        advance s 
        match current s with
        | Some '+' -> advance s; PLUSPLUS;
        | _ -> PLUS

    | Some '*' -> advance s; STAR
    | Some '/' -> advance s; SLASH
    | Some '%' -> advance s; PERCENT

    | Some '=' ->
        advance s
        match current s with
        | Some '=' -> advance s; EQEQ
        | _ -> 
            lexError "Одиночный '=' не используется в Poof. Для сравнения используется '=='" line col

    | Some '!' ->
        advance s 
        match current s with
        | Some '=' -> advance s; NEQ
        | _ -> BANG

    | Some '<' ->
        advance s
        match current s with
        | Some '=' -> advance s; LTE
        | _ -> LT

    | Some '>' ->
        advance s
        match current s with 
        | Some '=' -> advance s; GTE
        | _ -> GT
    
    | Some '&' ->
        advance s
        match current s with
        | Some '&' -> advance s; AMPAMP
        | _ -> lexError "Ожидалось '&&'" line col

    | Some '|' ->
        advance s 
        match current s with
        | Some '|' -> advance s; PIPEPIPE
        | _ -> lexError "Одиночный '|' не поддерживается в Poof" line col

    | Some '(' -> advance s; LPAREN
    | Some ')' -> advance s; RPAREN
    | Some '[' -> advance s; LBRACKET
    | Some ']' -> advance s; RBRACKET
    | Some ',' -> advance s; COMMA

    | Some c ->
        lexError $"Неожиданный символ: '{c}'" line col 

// Токенизация всего исходного кода
let tokenize (source: string) : Token list =
    let state = createState source
    let tokens = System.Collections.Generic.List<Token>()

    let rec loop () = 
        let tok = nextToken state
        tokens.Add(tok)
        if tok <> EOF then loop ()
    
    loop()
    let result = tokens |> Seq.toList

    let rec clean = function
        | NEWLINE :: EOF :: rest -> EOF :: clean rest
        | x :: rest -> x :: clean rest
        | [] -> []

    clean result


// Вспомогательные функции для отладки
let printTokens (tokens: Token list) =
    tokens
    |> List.iteri (fun i tok ->
        printfn $" [{i:D3}] {tokenToString tok}")

let debugTokenize source = 
    printfn "---Токены Poof---"
    let tokens = tokenize source
    printTokens tokens
    printfn "-----------------"
    tokens

// Пример использования для тестов. Удалить
// [<EntryPoint>]
// let main _ =
//     let code = """
// ~~ пример программы на Poof ~~
// bind n to 5
// spell fact num ->
//   if num == 0
//     then 1
//     else num * (cast fact with num - 1)
// whisper (cast fact with n)
// """
//     debugTokenize code |> ignore
//     0