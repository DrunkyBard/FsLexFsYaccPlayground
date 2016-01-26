module ExpressionBuilder

open System.Linq.Expressions
open System.Reflection
open System
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let rec buildExpression (quote: Expr) : Expression = 
    match quote with
        | Value(obj, valType) when valType = typeof<float> -> Expression.Constant(obj, valType) :> Expression
        | Value(obj, valType) -> failwithf "Unsupported type parameter: %s" valType.AssemblyQualifiedName
        | Call(expr, mi: MethodInfo, argList) -> 
            let exprs = flattenExprList argList [] :> IEnumerable<Expression>
            Expression.Call(mi, exprs) :> Expression
        | any -> failwithf "Unexpected quotation node: \r\n %s" (any.ToString(true))

and flattenExprList exprList state = 
    match exprList with
        | [] -> List.rev state |> Seq.ofList :> IEnumerable<Expression>
        | h::t -> flattenExprList t (buildExpression h :: state)
    

let buildSin (arg: Expression) = 
    let sinMi = Array.find (fun (x : MethodInfo) -> x.Name = "Sin") (typeof<Math>.GetMethods())
    Expression.Call(sinMi, arg)

let buildCos (arg: Expression) = 
    let cosMi = Array.find (fun (x : MethodInfo) -> x.Name = "Cos") (typeof<Math>.GetMethods())
    Expression.Call(cosMi, arg)

let buildPower fArg sArg =
    let powMi = Array.find (fun (x : MethodInfo) -> x.Name = "Pow") (typeof<Math>.GetMethods())
    Expression.Call(powMi, fArg, sArg)

//let buildFact arg = 
//    let powMi = Array.find (fun (x : MethodInfo) -> x.Name = "Pow") (typeof<MathFunctions>.GetMethods())
//    let o = <@ MathFunctions.fact(1) @>
//    
//    Expression.Call(powMi, fArg, sArg)
