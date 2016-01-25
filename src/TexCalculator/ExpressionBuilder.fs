module ExpressionBuilder

open System.Linq.Expressions
open System.Reflection
open System

let buildSin (arg: Expression) = 
    let sinMi = Array.find (fun (x : MethodInfo) -> x.Name = "Sin") (typeof<Math>.GetMethods())
    Expression.Call(sinMi, arg)

let buildCos (arg: Expression) = 
    let cosMi = Array.find (fun (x : MethodInfo) -> x.Name = "Cos") (typeof<Math>.GetMethods())
    Expression.Call(cosMi, arg)

let buildPower fArg sArg =
    let powMi = Array.find (fun (x : MethodInfo) -> x.Name = "Pow") (typeof<Math>.GetMethods())
    Expression.Call(powMi, fArg, sArg)
