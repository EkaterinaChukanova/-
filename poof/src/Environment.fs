module Poof.Enviroment
open Poof.AST
open System
open System.Runtime.CompilerServices

// Тип value - результат вычисления выражения
[<CustomEquality; NoComparison>]
type Value =
    // Примитивные значения
    | VInt of int
    | VFloat of float
    | VBool of bool
    | VString of string
    | VUnit

    // Составные значения
    | VList of Value list

    // Пользовательская функция
    | VFunc of param: string * body: Expr * closure: Env 

    // Встроенный ритуал
    | VBuiltin of name: string * fn: (Value -> Value)

    // Ссылочная ячейка для поддержки рекурсии и изменяемого состояния
    | VRef of Value ref

    | VLazy of Lazy<Value>

    override this.Equals(obj) =
        match obj with
        | :? Value as other ->
            match this, other with
            | VInt a, VInt b -> a = b
            | VFloat a, VFloat b -> a = b
            | VBool a, VBool b -> a = b
            | VString a, VString b -> a = b
            | VUnit, VUnit -> true
            | VList a, VList b -> a = b
            | VFunc(p1,_,_), VFunc(p2,_,_) -> p1 = p2
            | VBuiltin(n1,_), VBuiltin(n2,_) -> n1 = n2
            | VRef r1, VRef r2 -> !r1 = !r2
            | VLazy a, VLazy b -> Object.ReferenceEquals(a, b)
            | _ -> false
        | _ -> false
    override this.GetHashCode() = 
        match this with 
        | VInt n -> hash n
        | VFloat f -> hash f
        | VBool b -> hash b
        | VString s -> hash s
        | VUnit -> 0
        | VList xs -> hash xs
        | VFunc(p,_,_) -> hash p
        | VBuiltin(n,_) -> hash n
        | VRef r -> hash !r
        | VLazy l -> RuntimeHelpers.GetHashCode(l :> obj)

// Тип Env - окружение выполнения
and Env =  {
    Bindings : Map<string, Value> // переменные текущего слоя
    Parent : Env option // родительское окружение
} 

// Операции над окружением

// Создать пустое окружение
let emptyEnv : Env = 
    { Bindings = Map.empty; Parent = None }

// Создать новый слой поверх существующего окружения
let extendEnv (parent: Env) : Env = 
    { Bindings = Map.empty; Parent = Some parent }

// Добавить привязку имя->значение в текущий слой окружения
let bindValue (name: string) (value: Value) (env: Env) : Env = 
    let refValue = VRef (ref value)
    { env with Bindings = env.Bindings |> Map.add name refValue }

// Найти значение по имени - ищет снизу вверх по цепочке
let rec lookupValue (name: string) (env: Env) : Value option = 
    match env.Bindings |> Map.tryFind name with 
    | Some (VRef r) -> Some !r
    | Some value -> Some value
    | None ->
        match env.Parent with
        | Some parent -> lookupValue name parent
        | None -> None 

let lookupOrFail (name: string) (env: Env) : Value =
    match lookupValue name env with 
    | Some (VRef r) -> !r
    | Some v -> v
    | None -> failwith $"The name '{name}' is unknown - no such spell has been bound in this realm"

// Печать значений

let rec valueToString (value: Value) : string = 
    match value with
    | VInt n -> string n
    | VFloat f ->
        let s = sprintf "%g" f 
        if s.Contains('.') || s.Contains('e') then s else s + ".0"
    | VBool true -> "true"
    | VBool false -> "false"
    | VString s -> s
    | VUnit -> "()"
    | VRef r -> valueToString !r
    | VList items ->
        let inner = items |> List.map valueToString |> String.concat ", "
        $"[{inner}]"
    | VFunc(param, _, _) ->
        $"<spell ({param} -> ...)>"
    | VBuiltin(name, _) ->
        $"<ritual {name}>"
    | VLazy _ ->
        "<lazy>"

let rec valueToDebugString (value: Value) : string = 
    match value with
    | VString s -> $"\"{s}\""
    | VList items ->
        let inner = items |> List.map valueToDebugString |> String.concat ", "
        $"[{inner}]"
    | other -> valueToString other

// Ошибка времени выполнения
// Отдельный тип для runtime-ошибок
exception RuntimeError of message: string
let runtimeError msg = raise (RuntimeError msg)

let requireInt (v: Value) = 
    match v with
    | VInt n -> n
    | _ -> runtimeError $"An integer was required for this ritual, but received: '{valueToString v}'"

let requireFloat (v: Value) =
    match v with
    | VFloat f -> f
    | VInt n -> float n
    | _ -> runtimeError $"A float was required for this ritual, but received: '{valueToString v}'"

let requireBool (v: Value) =
    match v with
    | VBool b -> b
    | _ -> runtimeError $"A truth value (true/false) was required, but received: '{valueToString v}'"

let requireString (v: Value) =
    match v with
    | VString s -> s
    | _ -> runtimeError $"A string was required for this ritual, but received: '{valueToString v}'"

let requireList (v: Value) =
    match v with
    | VList items -> items
    | _ -> runtimeError $"A list was required for this ritual, but received: '{valueToString v}'"

let requireFunc (v: Value) =
    match v with
    | VFunc(p, b, e) -> (p, b, e)
    | VBuiltin(n, f) ->
        runtimeError $"Expected a user spell, but received built-in ritual: '{n}'"
    | _ -> runtimeError $"'{valueToString v}' is not a spell — it cannot be cast"

// Глобальное окружение - встроенные функции Poof
// Начальное окружение, которое получает каждая программа
let globalEnv : Env = 
    emptyEnv
    // Арифметические функции
    // очистить число от знака (abs)
    |> bindValue "purify" (VBuiltin("purify", fun v ->
        match v with
        | VInt n -> VInt (abs n)
        | VFloat f -> VFloat (abs f)
        | _ -> runtimeError $"'purify' can only cleanse numbers, not '{valueToString v}'"))
    
    // изменить знак числа
    |> bindValue "negate" (VBuiltin("negate", fun v ->
        match v with
        | VInt n -> VInt (-n)
        | VFloat f -> VFloat (-f)
        | _ -> runtimeError $"'negate' can only invert numbers, not '{valueToString v}'"))

    // Логические функции
    // логическое отрицание
    |> bindValue "not" (VBuiltin("not", fun v ->
        match v with
        | VBool b -> VBool (not b)
        | _ -> runtimeError $"'not' can only negate truth values, not '{valueToString v}'"))

    // Ритуалы над списками
    // взглянуть на первый элемент (head)
    |> bindValue "peek" (VBuiltin("peek", fun v ->
        match requireList v with
        | [] -> runtimeError "'peek' cannot gaze into the void — the list is empty"
        | x :: _ -> x))

    // получить хвост списка (tail)
    |> bindValue "rest" (VBuiltin("rest", fun v ->
        match requireList v with
        | [] -> runtimeError "'rest' finds nothing to return — the list is empty"
        | _ :: t -> VList t))

    // добавить элемент в начало (cons)
    |> bindValue "conjure" (VBuiltin("conjure", fun head ->
        VBuiltin("conjure$1", fun tail ->
            VList (head :: requireList tail))))

    // посчитать элементы (length)
    |> bindValue "count" (VBuiltin("count", fun v ->
        VInt (requireList v |> List.length)))

    // проверить, пуст ли список (isEmpty)
    |> bindValue "isVoid" (VBuiltin("isVoid", fun v ->
        VBool (requireList v |> List.isEmpty)))
    
    // отразить список (reverse)
    |> bindValue "mirror" (VBuiltin("mirror", fun v ->
        VList (requireList v |> List.rev)))

    // слить два списка (append)
    |> bindValue "merge" (VBuiltin("merge", fun a ->
        VBuiltin("merge$1", fun b ->
            VList (requireList a @ requireList b))))

    // Функции высшего порядка
    // передаем aaplyFn как параметр при инициализации

    // Заглушки - будут заменены в initGlobalEnv

    // map
    |> bindValue "enchant" (VBuiltin("enchant", fun _ ->
        runtimeError "'enchant' ritual is not yet awakened - call initGlobalEnv first"))
    // filter
    |> bindValue "sieve" (VBuiltin("sieve", fun _ ->
        runtimeError "'sieve' ritual is not yet awakened - call initGlobalEnv first"))
    // fold
    |> bindValue "condense" (VBuiltin("condense", fun _ ->
        runtimeError "'condense' ritual is not yet awakened - call initGlobalEnv first"))
    // any
    |> bindValue "anyMatch" (VBuiltin("anyMatch", fun _ ->
        runtimeError "'anyMatch' ritual is not yet awakened - call initGlobalEnv first"))
    // all
    |> bindValue "allMatch" (VBuiltin("allMatch", fun _ ->
        runtimeError "'allMatch' ritual is not yet awakened - call initGlobalEnv first"))
    // forEach
    |> bindValue "ritual" (VBuiltin("ritual", fun _ ->
        runtimeError "'ritual' is not yet awakened - call initGlobalEnv first"))

    // Функции ввода-вывода

    // тихо вывести без переноса строки (print)
    |> bindValue "whisper" (VBuiltin("whisper", fun v ->
        printf "%s" (valueToString v)
        VUnit))

    // громко объявить с переносом строки (println)
    |> bindValue "announce" (VBuiltin("announce", fun v ->
        printfn "%s" (valueToString v)
        VUnit))

    // услышать голос пользователя (readLine)
    |> bindValue "listen" (VBuiltin("listen", fun _ ->
        let line = System.Console.ReadLine()
        VString (if line = null then "" else line)))

    // Преобразование типов

    // toString
    |> bindValue "inscribe" (VBuiltin("inscribe", fun v ->
        VString (valueToString v)))

    // toInt
    |> bindValue "transmute" (VBuiltin("transmute", fun v ->
        match v with
        | VInt n -> VInt n
        | VFloat f -> VInt (int f)
        | VString s ->
            match System.Int32.TryParse(s) with
            | true, n -> VInt n
            | _ -> runtimeError $"'transmute' failed - '{s}' cannot be transmuted into an integer"
        | _ -> runtimeError $"'transmute' failed - '{valueToString v}' resists transformation"))

    // растворить в дробное число (toFloat)
    |> bindValue "dissolve" (VBuiltin("dissolve", fun v ->
        match v with
        | VFloat f -> VFloat f
        | VInt n -> VFloat (float n)
        | VString s ->
            match System.Double.TryParse(s) with
            | true, f -> VFloat f
            | _ -> runtimeError $"'dissolve' failed — '{s}' cannot be dissolved into a float"
        | _ -> runtimeError $"'dissolve' failed — '{valueToString v}' resists dissolution"))

    // Математические ритуалы

    // sqrt
    |> bindValue "unroot" (VBuiltin("unroot", fun v ->
        VFloat (sqrt (requireFloat v))))

    |> bindValue "floor" (VBuiltin("floor", fun v ->
        VInt (int (floor (requireFloat v)))))

    |> bindValue "ceil" (VBuiltin("ceil", fun v ->
        VInt  (int (ceil (requireFloat v)))))

    |> bindValue "mod" (VBuiltin("mod", fun a ->
        VBuiltin("mod$1", fun b ->
            VInt (requireInt a % requireInt b))))

    // Строковые заклинания

    // strLength
    |> bindValue "measure" (VBuiltin("measure", fun v ->
        VInt (requireString v |> String.length)))

    // содержит ли строка подстроку
    |> bindValue "contains" (VBuiltin("contains", fun haystack ->
        VBuiltin("contains$1", fun needle ->
            VBool ((requireString haystack).Contains(requireString needle)))))

// Инициализация с функциями высшего порядка
let initGlobalEnv (applyFn: Value -> Value -> Value) : Env =
    globalEnv

    // наложить чары на каждый элемент (map)
    |> bindValue "enchant" (VBuiltin("enchant", fun f ->
        VBuiltin("enchant$1", fun lst ->
            VList (requireList lst |> List.map (applyFn f)))))
        
    // просеять список через предикат (filter)
    |> bindValue "sieve" (VBuiltin("sieve", fun f ->
        VBuiltin("sieve$1", fun lst ->
            VList (requireList lst |> List.filter (fun x ->
                requireBool (applyFn f x))))))

    // свернуть список в одно значение (fold)
    |> bindValue "condense" (VBuiltin("condense", fun f ->
        VBuiltin("condense$1", fun acc ->
            VBuiltin("condense$2", fun lst ->
                requireList lst |> List.fold (fun a x ->
                    applyFn (applyFn f a) x) acc))))

    // есть ли хоть один элемент, удовлетворяющий предикату
    |> bindValue "anyMatch" (VBuiltin("anyMatch", fun f ->
        VBuiltin("anyMatch$1", fun lst ->
            VBool (requireList lst |> List.exists (fun x ->
                requireBool (applyFn f x))))))

    // все ли элементы удовлетворяют предикату
    |> bindValue "allMatch" (VBuiltin("allMatch", fun f ->
        VBuiltin("allMatch$1", fun lst ->
            VBool (requireList lst |> List.forall (fun x ->
                requireBool (applyFn f x))))))

    // выполнить ритуал над каждым эелемнтом (forEach)
    |> bindValue "ritual" (VBuiltin("ritual", fun f ->
        VBuiltin("ritual$1", fun lst ->
            requireList lst |> List.iter (fun x -> applyFn f x |> ignore)
            VUnit)))

    |> bindValue "delay" (VBuiltin("delay", fun f ->
        VLazy (lazy (applyFn f VUnit))))

    |> bindValue "force" (VBuiltin("force", fun v ->
        match v with
        | VLazy lz -> lz.Force()
        | _ -> runtimeError "force expects a lazy value produced by delay"))
