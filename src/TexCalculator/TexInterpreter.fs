module TexInterpreter

open System
open TexAst
open MathNet.Numerics
open System.Linq.Expressions

let rec execute (ast: TexAst.Expr) : float = 
    match ast with
        | Plus(e1, e2) -> execute(e1) + execute(e2)
        | Mul (e1, e2) -> execute(e1) * execute(e2)
        | Sub (e1, e2) -> execute(e1) - execute(e2)
        | Div (e1, e2) -> execute(e1) / execute(e2)
        | Pow (e1, e2) -> System.Math.Pow(float(execute(e1)), float(execute(e2)))
        | Int(x) -> float(x)
        | Constant(Pi) -> System.Math.PI
        | Constant(E) -> System.Math.E
        | Neg(e) -> -1. * execute(e)
        | Integral(func, diffVar, lowBE, uppBE) -> 
            let diffParameter = Expression.Parameter(typeof<float>, diffVar)
            let expr = buildExpression diffParameter func
            let lam = Expression.Lambda<Func<float, float>>(expr, diffParameter)
            let lamFunc = lam.Compile()
            Integrate.OnClosedInterval(lamFunc, execute(lowBE), execute(uppBE))
//        | Sum(idxId, upperBoundId, exprs) -> 
//            match upperBoundId with
//                | Some(id)
//            let evalExprs = List.map execute exprs

and buildExpression lamParameter expr : Expression = 
    match expr with
        | Plus(e1, e2) -> 
            let v1 = buildExpression lamParameter e1
            let v2 = buildExpression lamParameter e2
            Expression.Add(v1, v2) :> Expression
        | Sub(e1, e2) -> 
            let v1 = buildExpression lamParameter e1
            let v2 = buildExpression lamParameter e2
            Expression.Subtract(v1, v2) :> Expression
        | Mul(e1, e2) -> 
            let v1 = buildExpression lamParameter e1
            let v2 = buildExpression lamParameter e2
            Expression.Multiply(v1, v2) :> Expression
        | Div(e1, e2) -> 
            let v1 = buildExpression lamParameter e1
            let v2 = buildExpression lamParameter e2
            Expression.Divide(v1, v2) :> Expression
        | Var(x) -> 
            if x <> lamParameter.Name then failwithf "Incorrect parameter. Expected: '%s'. Actual: '%s'" lamParameter.Name x
            else lamParameter :> Expression
        | expr -> Expression.Constant(execute expr) :> Expression

