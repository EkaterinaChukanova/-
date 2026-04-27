module Poof.Program
// open Poof.Lexer

// [<EntryPoint>]
// let main _ =
//     let code = """
// ~~ тест лексера ~~
// bind n to 5
// spell fact num ->
//   if num == 0
//     then 1
//     else num * (cast fact with num - 1)
// whisper (cast fact with n)
// """
//     debugTokenize code |> ignore
//     0

// open Poof.Parser

// [<EntryPoint>]
// let main _ =
//     let code = """
// spell fact n ->
//   if n == 0
//     then 1
//     else n * (cast fact with n - 1)
// whisper (cast fact with 5)
// """
//     debugParse code |> ignore
//     0

open Poof.Tests
open Poof.Interpreter
open Poof.Enviroment

[<EntryPoint>]
let main argv =
    if argv |> Array.contains "--test" then
        let ok = runAllTests ()
        if ok then 0 else 1   // код возврата 1 если тесты упали
    else
        // обычный запуск программы
        let code = System.Console.In.ReadToEnd()
        let result = run code
        if result <> VUnit then
            printfn "%s" (valueToString result)
        0
        