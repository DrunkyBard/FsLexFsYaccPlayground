module DomainSpecificInterpreter

open QueryStatement

let dataStorage = 
    [
        "A", [
                "A1", [ "IntProp", 1.; "ValProp", 8. ] |> Map.ofList;
                "A2", [ "IntProp", 2.; "ValProp", 2. ] |> Map.ofList;
                "A3", [ "IntProp", 3.; "ValProp", 3. ] |> Map.ofList
             ] |> Map.ofList;
        "B", [
                "B1", [ "IntProp", 1.; "ValProp", 1. ] |> Map.ofList;
                "B2", [ "IntProp", 2.; "ValProp", 2. ] |> Map.ofList;
                "B3", [ "IntProp", 3.; "ValProp", 3. ] |> Map.ofList
             ] |> Map.ofList;
        "C", [
                "C1", [ "IntProp", 1.; "ValProp", 1. ] |> Map.ofList;
                "C2", [ "IntProp", 2.; "ValProp", 2. ] |> Map.ofList;
                "C3", [ "IntProp", 3.; "ValProp", 3. ] |> Map.ofList
             ] |> Map.ofList;
    ] |> Map.ofList

let rec buildExp (whereStatement: WhereStatement) =
    match whereStatement with
        | Operation(op, k, String(s)) ->
            match op with
                | Eq  -> fun key _ -> key = s
                | Neq -> fun key _ -> key <> s
                | Lt  -> fun key _ -> key < s
                | Le  -> fun key _ -> key <= s
                | Gt  -> fun key _ -> key > s
                | Ge  -> fun key _ -> key >= s
        | And(op1, op2) -> fun key value -> buildExp op1 key value && buildExp op2 key value
        | Or(op1, op2) -> fun key value -> buildExp op1 key value || buildExp op2 key value
        | any -> failwithf "Unsupported expression: %A" any

let evalSValue (ast: obj) : float = 
    match ast :?> Statement with
        | {For = forStatement; Select = selectStatement; Type = Single(id, String(value))} -> 
            (Map.find forStatement >> Map.find value >> Map.find selectStatement) dataStorage
        | _ -> failwithf "Unexpected Multi value statement for next AST: \r\n %A" ast

let evalMValue (ast: obj) : float list = 
    match ast :?> Statement with
        | { For = forStatement; Select = selectStatement; Type = Many(whereStatement) } -> 
            let dataFilterComposition = 
                Map.find forStatement 
                >> Map.filter (buildExp(whereStatement)) 
                >> Map.toList
                >> List.map snd
                >> List.map (fun m -> Map.find selectStatement m)
            dataFilterComposition dataStorage
        | _ -> failwithf "Unexpected Single value statement for next AST: \r\n %A" ast



