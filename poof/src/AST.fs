module Poof.AST

type Expr = 
    // Литералы
    | IntLit of int
    | FloatLit of float
    | BoolLit of bool
    | StringLit of string
    | ListLit of Expr list
    | Unit

    // Переменные
    | Var of string

    // Связывание
    | Bind of string * Expr * Expr

    // Функции
    | Spell of string * string * Expr

    // Анонимная функция
    | Lambda of string * Expr

    // Применение функций
    | Apply of Expr * Expr
    | Cast of string * Expr
    | Invoke of Expr * Expr * Expr

    // Упправление потоком
    | If of Expr * Expr * Expr
    | When of Expr * (Pattern * Expr) list

    // Операторы
    | BinOp of string * Expr * Expr
    | UnariMinus of Expr

    // Ввод/вывод
    | Whisper of Expr

    // Последовательность
    | Sequence of Expr list

// Патерны для when/is
and Pattern =
    | PInt of int
    | PFloat of float
    | PBool of bool
    | PString of string
    | PVar of string
    | PWild
    | PList of Pattern list