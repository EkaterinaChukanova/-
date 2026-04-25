// ============================================================
//  Tests.fs — тесты лексера и парсера языка Poof
// ============================================================
//
//  Запуск: dotnet run --project poof.fsproj -- --test
//
//  Тесты проверяют:
//    1. Лексер — правильно ли токенизируются все конструкции
//    2. Парсер — правильно ли строится AST
//    3. Граничные случаи и обработка ошибок
//
// ============================================================

module Poof.Tests

open Poof.AST
open Poof.Lexer
open Poof.Parser
open Poof.Enviroment


// ============================================================
//  1. ИНФРАСТРУКТУРА ТЕСТОВ
// ============================================================

// Счётчики для итогового отчёта
let mutable passed = 0
let mutable failed = 0
let mutable total  = 0

/// Запустить один тест.
let test (name: string) (testFn: unit -> bool) =
    total <- total + 1
    try
        if testFn () then
            passed <- passed + 1
            printfn "  [OK] %s" name
        else
            failed <- failed + 1
            printfn "  [FAIL] %s" name
    with ex ->
        failed <- failed + 1
        printfn "  [ERROR] %s" name
        printfn "          %s" ex.Message

/// Запустить тест который должен бросить исключение.
let testFails (name: string) (testFn: unit -> unit) =
    total <- total + 1
    try
        testFn () |> ignore
        failed <- failed + 1
        printfn "  [FAIL] %s (должна была быть ошибка, но её нет)" name
    with _ ->
        passed <- passed + 1
        printfn "  [OK] %s (ошибка поймана верно)" name

/// Напечатать итоговый отчёт
let printSummary () =
    printfn ""
    printfn "================================"
    printfn "  Итого: %d/%d тестов прошло" passed total
    if failed > 0 then
        printfn "  Провалено: %d" failed
    printfn "================================"
    failed = 0  // возвращает true если все тесты прошли


// ============================================================
//  2. ТЕСТЫ ЛЕКСЕРА
// ============================================================

let runLexerTests () =
    printfn ""
    printfn "--- Тесты лексера ---"

    //  Ключевые слова
    test "ключевые слова Poof" (fun () ->
        let tokens = tokenize "bind to spell cast with invoke on whisper"
        tokens = [BIND; TO; SPELL; CAST; WITH; INVOKE; ON; WHISPER; EOF])

    test "управляющие конструкции" (fun () ->
        let tokens = tokenize "if then else when is"
        tokens = [IF; THEN; ELSE; WHEN; IS; EOF])

    test "булевы литералы" (fun () ->
        let tokens = tokenize "true false"
        tokens = [TRUE; FALSE; EOF])

    //  Литералы
    test "целое число" (fun () ->
        tokenize "42" = [INT 42; EOF])

    test "отрицательное число через минус" (fun () ->
        tokenize "- 7" = [MINUS; INT 7; EOF])

    test "дробное число" (fun () ->
        tokenize "3.14" = [FLOAT 3.14; EOF])

    test "дробное число 2.0" (fun () ->
        tokenize "2.0" = [FLOAT 2.0; EOF])

    test "строка простая" (fun () ->
        tokenize "\"hello\"" = [STRING "hello"; EOF])

    test "строка с пробелами" (fun () ->
        tokenize "\"hello world\"" = [STRING "hello world"; EOF])

    test "строка с escape \\n" (fun () ->
        tokenize "\"line1\\nline2\"" = [STRING "line1\nline2"; EOF])

    test "строка с escape \\t" (fun () ->
        tokenize "\"tab\\there\"" = [STRING "tab\there"; EOF])

    test "строка с escape кавычка" (fun () ->
        tokenize "\"say \\\"hi\\\"\"" = [STRING "say \"hi\""; EOF])

    test "пустая строка" (fun () ->
        tokenize "\"\"" = [STRING ""; EOF])

    //  Идентификаторы
    test "простой идентификатор" (fun () ->
        tokenize "myVar" = [IDENT "myVar"; EOF])

    test "идентификатор с цифрами" (fun () ->
        tokenize "x1" = [IDENT "x1"; EOF])

    test "идентификатор с подчёркиванием" (fun () ->
        tokenize "my_spell" = [IDENT "my_spell"; EOF])

    test "идентификатор начинается с подчёркивания" (fun () ->
        tokenize "_helper" = [IDENT "_helper"; EOF])

    test "wildcard _ отдельно" (fun () ->
        tokenize "_" = [UNDERSCORE; EOF])

    test "wildcard не путается с идентификатором" (fun () ->
        tokenize "_ _foo" = [UNDERSCORE; IDENT "_foo"; EOF])

    //  Операторы
    test "стрелка ->" (fun () ->
        tokenize "->" = [ARROW; EOF])

    test "минус не путается со стрелкой" (fun () ->
        tokenize "x - y" = [IDENT "x"; MINUS; IDENT "y"; EOF])

    test "плюс и плюсплюс" (fun () ->
        tokenize "+ ++" = [PLUS; PLUSPLUS; EOF])

    test "оператор ==" (fun () ->
        tokenize "==" = [EQEQ; EOF])

    test "оператор !=" (fun () ->
        tokenize "!=" = [NEQ; EOF])

    test "оператор <" (fun () ->
        tokenize "<" = [LT; EOF])

    test "оператор >" (fun () ->
        tokenize ">" = [GT; EOF])

    test "оператор <=" (fun () ->
        tokenize "<=" = [LTE; EOF])

    test "оператор >=" (fun () ->
        tokenize ">=" = [GTE; EOF])

    test "логические операторы" (fun () ->
        tokenize "&& ||" = [AMPAMP; PIPEPIPE; EOF])

    test "логическое НЕ" (fun () ->
        tokenize "!" = [BANG; EOF])

    test "арифметика" (fun () ->
        tokenize "+ - * / %" = [PLUS; MINUS; STAR; SLASH; PERCENT; EOF])

    //  Пунктуация
    test "скобки" (fun () ->
        tokenize "( )" = [LPAREN; RPAREN; EOF])

    test "квадратные скобки" (fun () ->
        tokenize "[ ]" = [LBRACKET; RBRACKET; EOF])

    test "запятая" (fun () ->
        tokenize "," = [COMMA; EOF])

    //  Пробелы и переносы строк
    test "пробелы игнорируются" (fun () ->
        tokenize "   bind   x   " = [BIND; IDENT "x"; EOF])

    test "табуляция игнорируется" (fun () ->
        tokenize "\tbind\tx" = [BIND; IDENT "x"; EOF])

    test "перенос строки — токен NEWLINE" (fun () ->
        tokenize "x\ny" = [IDENT "x"; NEWLINE; IDENT "y"; EOF])

    test "несколько переносов — один NEWLINE" (fun () ->
        tokenize "x\n\n\ny" = [IDENT "x"; NEWLINE; IDENT "y"; EOF])

    test "NEWLINE перед EOF убирается" (fun () ->
        tokenize "x\n" = [IDENT "x"; EOF])

    //  Комментарии
    test "комментарий ~~ пропускается" (fun () ->
        tokenize "~~ это комментарий ~~\nx" = [NEWLINE; IDENT "x"; EOF])

    test "комментарий до конца строки" (fun () ->
        tokenize "x ~~ комментарий\ny" = [IDENT "x"; NEWLINE; IDENT "y"; EOF])

    test "только комментарий" (fun () ->
        tokenize "~~ пусто ~~" = [EOF])

    //  Сложные случаи
    test "выражение bind x to 42" (fun () ->
        tokenize "bind x to 42" = [BIND; IDENT "x"; TO; INT 42; EOF])

    test "список [1, 2, 3]" (fun () ->
        tokenize "[1, 2, 3]" = [LBRACKET; INT 1; COMMA; INT 2; COMMA; INT 3; RBRACKET; EOF])

    test "spell с несколькими параметрами" (fun () ->
        tokenize "spell add a b ->" = [SPELL; IDENT "add"; IDENT "a"; IDENT "b"; ARROW; EOF])

    //  Ошибки лексера
    testFails "неизвестный символ @" (fun () ->
        tokenize "@" |> ignore)

    testFails "незакрытая строка" (fun () ->
        tokenize "\"hello" |> ignore)

    testFails "одиночный = вместо ==" (fun () ->
        tokenize "=" |> ignore)


// ============================================================
//  3. ТЕСТЫ ПАРСЕРА
// ============================================================

let runParserTests () =
    printfn ""
    printfn "--- Тесты парсера ---"

    //  Литералы
    test "парсинг целого числа" (fun () ->
        parseString "42" = IntLit 42)

    test "парсинг дробного числа" (fun () ->
        parseString "3.14" = FloatLit 3.14)

    test "парсинг true" (fun () ->
        parseString "true" = BoolLit true)

    test "парсинг false" (fun () ->
        parseString "false" = BoolLit false)

    test "парсинг строки" (fun () ->
        parseString "\"hello\"" = StringLit "hello")

    test "парсинг Unit ()" (fun () ->
        parseString "()" = Unit)

    test "парсинг переменной" (fun () ->
        parseString "x" = Var "x")

    //  Списки
    test "пустой список" (fun () ->
        parseString "[]" = ListLit [])

    test "список из одного элемента" (fun () ->
        parseString "[42]" = ListLit [IntLit 42])

    test "список из трёх элементов" (fun () ->
        parseString "[1, 2, 3]" = ListLit [IntLit 1; IntLit 2; IntLit 3])

    test "список строк" (fun () ->
        parseString "[\"a\", \"b\"]" = ListLit [StringLit "a"; StringLit "b"])

    test "вложенный список" (fun () ->
        parseString "[[1, 2], [3, 4]]" =
            ListLit [ListLit [IntLit 1; IntLit 2]; ListLit [IntLit 3; IntLit 4]])

    //  Бинарные операторы и приоритеты
    test "сложение" (fun () ->
        parseString "1 + 2" = BinOp("+", IntLit 1, IntLit 2))

    test "вычитание" (fun () ->
        parseString "5 - 3" = BinOp("-", IntLit 5, IntLit 3))

    test "умножение" (fun () ->
        parseString "2 * 3" = BinOp("*", IntLit 2, IntLit 3))

    test "деление" (fun () ->
        parseString "10 / 2" = BinOp("/", IntLit 10, IntLit 2))

    test "остаток" (fun () ->
        parseString "7 % 3" = BinOp("%", IntLit 7, IntLit 3))

    test "приоритет: * выше +" (fun () ->
        // 2 + 3 * 4 должно быть 2 + (3 * 4)
        parseString "2 + 3 * 4" =
            BinOp("+", IntLit 2, BinOp("*", IntLit 3, IntLit 4)))

    test "приоритет: скобки меняют порядок" (fun () ->
        // (2 + 3) * 4
        parseString "(2 + 3) * 4" =
            BinOp("*", BinOp("+", IntLit 2, IntLit 3), IntLit 4))

    test "конкатенация строк ++" (fun () ->
        parseString "\"hello\" ++ \" world\"" =
            BinOp("++", StringLit "hello", StringLit " world"))

    test "сравнение ==" (fun () ->
        parseString "x == 0" = BinOp("==", Var "x", IntLit 0))

    test "сравнение !=" (fun () ->
        parseString "x != 0" = BinOp("!=", Var "x", IntLit 0))

    test "сравнение <" (fun () ->
        parseString "x < 10" = BinOp("<", Var "x", IntLit 10))

    test "сравнение >=" (fun () ->
        parseString "x >= 0" = BinOp(">=", Var "x", IntLit 0))

    test "логическое И &&" (fun () ->
        parseString "true && false" = BinOp("&&", BoolLit true, BoolLit false))

    test "логическое ИЛИ ||" (fun () ->
        parseString "true || false" = BinOp("||", BoolLit true, BoolLit false))

    test "приоритет: && выше ||" (fun () ->
        // a || b && c  =  a || (b && c)
        parseString "a || b && c" =
            BinOp("||", Var "a", BinOp("&&", Var "b", Var "c")))

    //  Лямбды
    test "простая лямбда" (fun () ->
        parseString "x -> x" = Lambda("x", Var "x"))

    test "лямбда с арифметикой" (fun () ->
        parseString "x -> x * 2" = Lambda("x", BinOp("*", Var "x", IntLit 2)))

    test "вложенная лямбда" (fun () ->
        parseString "a -> b -> a + b" =
            Lambda("a", Lambda("b", BinOp("+", Var "a", Var "b"))))

    //  Применение функций
    test "применение функции к одному аргументу" (fun () ->
        parseString "f x" = Apply(Var "f", Var "x"))

    test "применение к двум аргументам (лево-ассоциативно)" (fun () ->
        // f x y = (f x) y
        parseString "f x y" = Apply(Apply(Var "f", Var "x"), Var "y"))

    test "применение к литералу" (fun () ->
        parseString "f 42" = Apply(Var "f", IntLit 42))

    test "применение в скобках" (fun () ->
        parseString "(f x)" = Apply(Var "f", Var "x"))

    //  if / then / else
    test "if/then/else с литералами" (fun () ->
        parseString "if true then 1 else 0" =
            If(BoolLit true, IntLit 1, IntLit 0))

    test "if/then/else с выражениями" (fun () ->
        parseString "if n == 0 then 1 else n * 2" =
            If(BinOp("==", Var "n", IntLit 0),
               IntLit 1,
               BinOp("*", Var "n", IntLit 2)))

    test "вложенный if" (fun () ->
        parseString "if a then if b then 1 else 2 else 3" =
            If(Var "a", If(Var "b", IntLit 1, IntLit 2), IntLit 3))

    //  cast
    test "cast простой" (fun () ->
        parseString "cast fact with 5" =
            Cast("fact", IntLit 5))

    test "cast с выражением" (fun () ->
        parseString "cast fact with n - 1" =
            Cast("fact", BinOp("-", Var "n", IntLit 1)))

    test "cast в скобках" (fun () ->
        parseString "(cast fact with n - 1)" =
            Cast("fact", BinOp("-", Var "n", IntLit 1)))

    //  invoke
    test "invoke простой" (fun () ->
        parseString "invoke map with f on xs" =
            Invoke(Var "map", Var "f", Var "xs"))

    test "invoke с лямбдой" (fun () ->
        parseString "invoke map with (x -> x * 2) on nums" =
            Invoke(Var "map",
                   Lambda("x", BinOp("*", Var "x", IntLit 2)),
                   Var "nums"))

    //  bind
    test "bind простой" (fun () ->
        parseString "bind x to 42" =
            Bind("x", IntLit 42, Unit))

    test "bind строки" (fun () ->
        parseString "bind name to \"Poof\"" =
            Bind("name", StringLit "Poof", Unit))

    test "bind с выражением" (fun () ->
        parseString "bind result to 2 + 2" =
            Bind("result", BinOp("+", IntLit 2, IntLit 2), Unit))

    test "два bind подряд" (fun () ->
        parseString "bind x to 1\nbind y to 2" =
            Bind("x", IntLit 1, Bind("y", IntLit 2, Unit)))

    test "bind виден в следующей строке" (fun () ->
        // x должен быть виден в теле второго bind
        parseString "bind x to 5\nbind y to x" =
            Bind("x", IntLit 5, Bind("y", Var "x", Unit)))

    //  spell
    test "spell с одним параметром" (fun () ->
        parseString "spell double n -> n * 2" =
            Bind("double", Lambda("n", BinOp("*", Var "n", IntLit 2)), Unit))

    test "spell с двумя параметрами (каррирование)" (fun () ->
        // spell add a b -> a + b
        // должно стать: Bind("add", Lambda("a", Lambda("b", a+b)), Unit)
        parseString "spell add a b -> a + b" =
            Bind("add",
                 Lambda("a", Lambda("b", BinOp("+", Var "a", Var "b"))),
                 Unit))

    test "spell с тремя параметрами" (fun () ->
        parseString "spell f a b c -> a + b + c" =
            Bind("f",
                 Lambda("a", Lambda("b", Lambda("c",
                     BinOp("+", BinOp("+", Var "a", Var "b"), Var "c")))),
                 Unit))

    //  whisper
    test "whisper литерала" (fun () ->
        match parseString "whisper 42" with
        | Sequence [Whisper (IntLit 42); Unit] -> true
        | _ -> false)

    test "whisper строки" (fun () ->
        match parseString "whisper \"hello\"" with
        | Sequence [Whisper (StringLit "hello"); Unit] -> true
        | _ -> false)

    test "whisper выражения" (fun () ->
        match parseString "whisper (cast fact with 5)" with
        | Sequence [Whisper (Cast("fact", IntLit 5)); Unit] -> true
        | _ -> false)

    //  when / паттерн-матчинг
    test "when с числовыми паттернами" (fun () ->
        let code = "spell describe x ->\n  when x\n    0 -> \"zero\"\n    _ -> \"other\""
        match parseString code with
        | Bind("describe", Lambda("x", When(Var "x", cases)), Unit) ->
            cases.Length = 2
        | _ -> false)

    test "when с true/false" (fun () ->
        let code = "spell toBool x ->\n  when x\n    true -> 1\n    false -> 0"
        match parseString code with
        | Bind("toBool", Lambda("x", When(Var "x", cases)), Unit) ->
            cases.Length = 2
        | _ -> false)

    test "when с wildcard _" (fun () ->
        let code = "spell f x ->\n  when x\n    0 -> \"zero\"\n    1 -> \"one\"\n    _ -> \"many\""
        match parseString code with
        | Bind("f", Lambda("x", When(Var "x", cases)), Unit) ->
            cases.Length = 3 &&
            (match List.last cases with (PWild, _) -> true | _ -> false)
        | _ -> false)

    //  Полные программы
    test "факториал полностью" (fun () ->
        let code = """
spell fact n ->
  if n == 0
    then 1
    else n * (cast fact with n - 1)
whisper (cast fact with 5)
"""
        // Просто проверяем что парсится без ошибок и даёт Bind
        match parseString code with
        | Bind("fact", Lambda("n", If(_, _, _)), _) -> true
        | _ -> false)

    test "два spell подряд" (fun () ->
        let code = "spell double n -> n * 2\nspell triple n -> n * 3"
        match parseString code with
        | Bind("double", _, Bind("triple", _, Unit)) -> true
        | _ -> false)

    test "bind потом whisper" (fun () ->
        let code = "bind x to 42\nwhisper x"
        match parseString code with
        | Bind("x", IntLit 42, Sequence [Whisper (Var "x"); Unit]) -> true
        | _ -> false)

    test "пустая программа" (fun () ->
        parseString "" = Unit)

    test "только комментарий" (fun () ->
        parseString "~~ это комментарий ~~" = Unit)

    //  Ошибки парсера
    testFails "bind без to" (fun () ->
        parseString "bind x 42" |> ignore)

    testFails "spell без параметров" (fun () ->
        parseString "spell f -> 42" |> ignore)

    testFails "if без else" (fun () ->
        parseString "if true then 1" |> ignore)

    testFails "незакрытая скобка" (fun () ->
        parseString "(1 + 2" |> ignore)

    testFails "незакрытый список" (fun () ->
        parseString "[1, 2, 3" |> ignore)


// ============================================================
//  4. ТЕСТЫ ПАТТЕРНОВ
// ============================================================

let runPatternTests () =
    printfn ""
    printfn "--- Тесты паттернов ---"

    test "паттерн — целое число" (fun () ->
        let tokens = tokenize "0" |> List.toArray
        let s = { Tokens = tokens; Pos = 0 }
        parsePattern s = PInt 0)

    test "паттерн — строка" (fun () ->
        let tokens = tokenize "\"ok\"" |> List.toArray
        let s = { Tokens = tokens; Pos = 0 }
        parsePattern s = PString "ok")

    test "паттерн — true" (fun () ->
        let tokens = tokenize "true" |> List.toArray
        let s = { Tokens = tokens; Pos = 0 }
        parsePattern s = PBool true)

    test "паттерн — wildcard _" (fun () ->
        let tokens = tokenize "_" |> List.toArray
        let s = { Tokens = tokens; Pos = 0 }
        parsePattern s = PWild)

    test "паттерн — переменная" (fun () ->
        let tokens = tokenize "n" |> List.toArray
        let s = { Tokens = tokens; Pos = 0 }
        parsePattern s = PVar "n")


// ============================================================
//  5. СТРЕСС-ТЕСТЫ
// ============================================================

let runStressTests () =
    printfn ""
    printfn "--- Стресс-тесты ---"

    test "глубокая вложенность скобок" (fun () ->
        parseString "((((42))))" = IntLit 42)

    test "длинная цепочка сложений" (fun () ->
        parseString "1 + 2 + 3 + 4 + 5" =
            BinOp("+", BinOp("+", BinOp("+", BinOp("+",
                IntLit 1, IntLit 2), IntLit 3), IntLit 4), IntLit 5))

    test "много bind подряд" (fun () ->
        let code = "bind a to 1\nbind b to 2\nbind c to 3\nbind d to 4"
        match parseString code with
        | Bind("a", IntLit 1, Bind("b", IntLit 2,
               Bind("c", IntLit 3, Bind("d", IntLit 4, Unit)))) -> true
        | _ -> false)

    test "длинный список" (fun () ->
        let code = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
        match parseString code with
        | ListLit items -> items.Length = 10
        | _ -> false)

    test "spell с длинным телом" (fun () ->
        let code = """
spell compute a b c ->
  if a == 0
    then b * c
    else a + b + c
"""
        match parseString code with
        | Bind("compute", Lambda("a", Lambda("b", Lambda("c", If(_, _, _)))), Unit) -> true
        | _ -> false)

// ============================================================
//  ТЕСТЫ ENVIRONMENT
// ============================================================

let runEnvironmentTests () =
    printfn ""
    printfn "--- Тесты Environment ---"

    // ----------------------------------------------------------
    //  Базовые операции с окружением
    // ----------------------------------------------------------
    test "пустое окружение — переменная не найдена" (fun () ->
        lookupValue "x" emptyEnv = None)

    test "bindValue + lookupValue" (fun () ->
        let env = emptyEnv |> bindValue "x" (VInt 42)
        lookupValue "x" env = Some (VInt 42))

    test "два значения в одном слое" (fun () ->
        let env = emptyEnv
                  |> bindValue "x" (VInt 1)
                  |> bindValue "y" (VInt 2)
        lookupValue "x" env = Some (VInt 1) &&
        lookupValue "y" env = Some (VInt 2))

    test "новый слой перекрывает старый" (fun () ->
        let env1 = emptyEnv |> bindValue "x" (VInt 1)
        let env2 = extendEnv env1 |> bindValue "x" (VInt 99)
        lookupValue "x" env2 = Some (VInt 99))

    test "поиск уходит в родительский слой" (fun () ->
        let parent = emptyEnv |> bindValue "x" (VInt 42)
        let child  = extendEnv parent
        lookupValue "x" child = Some (VInt 42))

    test "lookupOrFail — нашёл" (fun () ->
        let env = emptyEnv |> bindValue "n" (VInt 5)
        lookupOrFail "n" env = VInt 5)

    testFails "lookupOrFail — не нашёл, бросает ошибку" (fun () ->
        lookupOrFail "ghost" emptyEnv |> ignore)

    // ----------------------------------------------------------
    //  Типы значений
    // ----------------------------------------------------------
    test "VInt сохраняется и читается" (fun () ->
        let env = emptyEnv |> bindValue "n" (VInt 42)
        lookupValue "n" env = Some (VInt 42))

    test "VFloat сохраняется и читается" (fun () ->
        let env = emptyEnv |> bindValue "f" (VFloat 3.14)
        lookupValue "f" env = Some (VFloat 3.14))

    test "VBool true" (fun () ->
        let env = emptyEnv |> bindValue "b" (VBool true)
        lookupValue "b" env = Some (VBool true))

    test "VString сохраняется" (fun () ->
        let env = emptyEnv |> bindValue "s" (VString "poof")
        lookupValue "s" env = Some (VString "poof"))

    test "VUnit сохраняется" (fun () ->
        let env = emptyEnv |> bindValue "u" VUnit
        lookupValue "u" env = Some VUnit)

    test "VList сохраняется" (fun () ->
        let lst = VList [VInt 1; VInt 2; VInt 3]
        let env = emptyEnv |> bindValue "xs" lst
        lookupValue "xs" env = Some lst)

    // ----------------------------------------------------------
    //  valueToString
    // ----------------------------------------------------------
    test "VInt → строка" (fun () ->
        valueToString (VInt 42) = "42")

    test "VFloat → строка" (fun () ->
        valueToString (VFloat 3.14) = "3.14")

    test "VBool true → строка" (fun () ->
        valueToString (VBool true) = "true")

    test "VBool false → строка" (fun () ->
        valueToString (VBool false) = "false")

    test "VString → строка без кавычек" (fun () ->
        valueToString (VString "hello") = "hello")

    test "VUnit → строка" (fun () ->
        valueToString VUnit = "()")

    test "VList → строка" (fun () ->
        valueToString (VList [VInt 1; VInt 2; VInt 3]) = "[1, 2, 3]")

    test "пустой VList → строка" (fun () ->
        valueToString (VList []) = "[]")

    test "VBuiltin → строка содержит имя ритуала" (fun () ->
        let s = valueToString (VBuiltin("enchant", fun x -> x))
        s.Contains("enchant"))

    // ----------------------------------------------------------
    //  require-функции
    // ----------------------------------------------------------
    test "requireInt успех" (fun () ->
        requireInt (VInt 5) = 5)

    testFails "requireInt провал на строке" (fun () ->
        requireInt (VString "x") |> ignore)

    test "requireBool успех" (fun () ->
        requireBool (VBool true) = true)

    testFails "requireBool провал на числе" (fun () ->
        requireBool (VInt 1) |> ignore)

    test "requireString успех" (fun () ->
        requireString (VString "hi") = "hi")

    testFails "requireString провал на числе" (fun () ->
        requireString (VInt 0) |> ignore)

    test "requireList успех" (fun () ->
        requireList (VList [VInt 1]) = [VInt 1])

    testFails "requireList провал на числе" (fun () ->
        requireList (VInt 0) |> ignore)

    test "requireFloat — int конвертируется в float" (fun () ->
        requireFloat (VInt 3) = 3.0)

    // ----------------------------------------------------------
    //  Встроенные ритуалы глобального окружения
    // ----------------------------------------------------------

    // Проверка наличия ритуалов
    test "глобальное окружение содержит peek" (fun () ->
        lookupValue "peek" globalEnv <> None)

    test "глобальное окружение содержит rest" (fun () ->
        lookupValue "rest" globalEnv <> None)

    test "глобальное окружение содержит not" (fun () ->
        lookupValue "not" globalEnv <> None)

    test "глобальное окружение содержит conjure" (fun () ->
        lookupValue "conjure" globalEnv <> None)

    test "глобальное окружение содержит count" (fun () ->
        lookupValue "count" globalEnv <> None)

    test "глобальное окружение содержит mirror" (fun () ->
        lookupValue "mirror" globalEnv <> None)

    test "глобальное окружение содержит purify" (fun () ->
        lookupValue "purify" globalEnv <> None)

    // not
    test "not true = false" (fun () ->
        match lookupOrFail "not" globalEnv with
        | VBuiltin(_, fn) -> fn (VBool true) = VBool false
        | _ -> false)

    test "not false = true" (fun () ->
        match lookupOrFail "not" globalEnv with
        | VBuiltin(_, fn) -> fn (VBool false) = VBool true
        | _ -> false)

    // peek (head)
    test "peek [1,2,3] = 1" (fun () ->
        match lookupOrFail "peek" globalEnv with
        | VBuiltin(_, fn) -> fn (VList [VInt 1; VInt 2; VInt 3]) = VInt 1
        | _ -> false)

    testFails "peek пустого списка — ошибка" (fun () ->
        match lookupOrFail "peek" globalEnv with
        | VBuiltin(_, fn) -> fn (VList []) |> ignore
        | _ -> ())

    // rest (tail)
    test "rest [1,2,3] = [2,3]" (fun () ->
        match lookupOrFail "rest" globalEnv with
        | VBuiltin(_, fn) -> fn (VList [VInt 1; VInt 2; VInt 3]) = VList [VInt 2; VInt 3]
        | _ -> false)

    testFails "rest пустого списка — ошибка" (fun () ->
        match lookupOrFail "rest" globalEnv with
        | VBuiltin(_, fn) -> fn (VList []) |> ignore
        | _ -> ())

    // count (length)
    test "count [1,2,3] = 3" (fun () ->
        match lookupOrFail "count" globalEnv with
        | VBuiltin(_, fn) -> fn (VList [VInt 1; VInt 2; VInt 3]) = VInt 3
        | _ -> false)

    test "count [] = 0" (fun () ->
        match lookupOrFail "count" globalEnv with
        | VBuiltin(_, fn) -> fn (VList []) = VInt 0
        | _ -> false)

    // isVoid (isEmpty)
    test "isVoid [] = true" (fun () ->
        match lookupOrFail "isVoid" globalEnv with
        | VBuiltin(_, fn) -> fn (VList []) = VBool true
        | _ -> false)

    test "isVoid [1] = false" (fun () ->
        match lookupOrFail "isVoid" globalEnv with
        | VBuiltin(_, fn) -> fn (VList [VInt 1]) = VBool false
        | _ -> false)

    // mirror (reverse)
    test "mirror [1,2,3] = [3,2,1]" (fun () ->
        match lookupOrFail "mirror" globalEnv with
        | VBuiltin(_, fn) ->
            fn (VList [VInt 1; VInt 2; VInt 3]) = VList [VInt 3; VInt 2; VInt 1]
        | _ -> false)

    // purify (abs)
    test "purify (-5) = 5" (fun () ->
        match lookupOrFail "purify" globalEnv with
        | VBuiltin(_, fn) -> fn (VInt -5) = VInt 5
        | _ -> false)

    test "purify 5 = 5" (fun () ->
        match lookupOrFail "purify" globalEnv with
        | VBuiltin(_, fn) -> fn (VInt 5) = VInt 5
        | _ -> false)

    // inscribe (toString)
    test "inscribe 42 = \"42\"" (fun () ->
        match lookupOrFail "inscribe" globalEnv with
        | VBuiltin(_, fn) -> fn (VInt 42) = VString "42"
        | _ -> false)

    test "inscribe true = \"true\"" (fun () ->
        match lookupOrFail "inscribe" globalEnv with
        | VBuiltin(_, fn) -> fn (VBool true) = VString "true"
        | _ -> false)

    // transmute (toInt)
    test "transmute float 3.9 = 3" (fun () ->
        match lookupOrFail "transmute" globalEnv with
        | VBuiltin(_, fn) -> fn (VFloat 3.9) = VInt 3
        | _ -> false)

    testFails "transmute строки не числа — ошибка" (fun () ->
        match lookupOrFail "transmute" globalEnv with
        | VBuiltin(_, fn) -> fn (VString "abc") |> ignore
        | _ -> ())

    // dissolve (toFloat)
    test "dissolve int 3 = 3.0" (fun () ->
        match lookupOrFail "dissolve" globalEnv with
        | VBuiltin(_, fn) -> fn (VInt 3) = VFloat 3.0
        | _ -> false)

    // unroot (sqrt)
    test "unroot 9 = 3.0" (fun () ->
        match lookupOrFail "unroot" globalEnv with
        | VBuiltin(_, fn) -> fn (VInt 9) = VFloat 3.0
        | _ -> false)

    // measure (strLength)
    test "measure \"hello\" = 5" (fun () ->
        match lookupOrFail "measure" globalEnv with
        | VBuiltin(_, fn) -> fn (VString "hello") = VInt 5
        | _ -> false)

// ============================================================
//  6. ТОЧКА ВХОДА
// ============================================================

let runAllTests () =
    printfn "==============================="
    printfn "  Тесты языка Poof"
    printfn "==============================="

    // Сбрасываем счётчики перед запуском
    passed <- 0
    failed <- 0
    total  <- 0

    runLexerTests ()
    runParserTests ()
    runPatternTests ()
    runStressTests ()
    runEnvironmentTests ()

    printSummary ()