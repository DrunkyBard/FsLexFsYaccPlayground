module TexAst

open System.Collections.Generic

type Token = 
    | INT of int
    | SUM
    | SUB
    | MUL
    | DIV
    | LPAREN
    | RPAREN
    | EOF

type Expr = 
    | Plus of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Sum of Expr list
    | Int of int

type Ast(parent: Ast option, node: Expr) = 
    member this.Node = node
    member this.Parent = parent

let processLowPriorOperation token (operands : Stack<Expr>) (operations : Stack<Token>) = 
    let rec processInternal token (operands : Stack<Expr>) (operations : Stack<Token>) = 
        match operations.Count with
        | 0 -> operations.Push(token)
        | _ -> 
            match operations.Peek() with
            | MUL -> 
                let sOp, fOp = operands.Pop(), operands.Pop()
                operands.Push(Mul(fOp, sOp))
                operations.Pop() |> ignore
                processInternal token operands operations
            | DIV -> 
                let sOp, fOp = operands.Pop(), operands.Pop()
                operands.Push(Mul(fOp, sOp))
                operations.Pop() |> ignore
                processInternal token operands operations
            | any -> operations.Push(any)
    processInternal token operands operations

let processHighPriorOperation token (operations : Stack<Token>) = operations.Push(token)

let rec eofTokenReached (operands : Stack<Expr>) (operations : Stack<Token>) : Expr = 
    match operands.Count, operations.Count with
    | (1, 0) -> operands.Pop()
    | (_, 0) -> failwith "Error"
    | (_, _) -> 
        let sOp, fOp = operands.Pop(), operands.Pop()
        let resOp = 
            match operations.Pop() with
            | SUM -> Plus(fOp, sOp)
            | SUB -> Sub(fOp, sOp)
            | MUL -> Mul(fOp, sOp)
            | DIV -> Div(fOp, sOp)
        operands.Push(resOp)
        eofTokenReached operands operations
