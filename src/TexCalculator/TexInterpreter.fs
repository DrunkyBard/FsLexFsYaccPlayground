module TexInterpreter

open System
open TexAst
open MathNet.Numerics
open System.Linq.Expressions
open ExpressionBuilder
open MathFunctions

type TexInterpreter (interpretSValue: obj -> float, interpretMValue: obj -> float list)=
    let interpretSValue = interpretSValue
    let interpretMValue = interpretMValue

    let rec execute (ast: TexAst.Expr)  : float = 
        match ast with
            | Plus(e1, e2) -> execute(e1) + execute(e2)
            | Mul (e1, e2) -> execute(e1) * execute(e2)
            | Sub (e1, e2) -> execute(e1) - execute(e2)
            | Div (e1, e2) -> execute(e1) / execute(e2)
            | Pow (e1, e2) -> Math.Pow(execute(e1), execute(e2))
            | Sqrt (e, powE) -> Math.Pow(execute(e), 1./execute(powE))
            | Sin(f, p) -> Math.Pow(Math.Sin(execute f), execute p)
            | Cos(f, p) -> Math.Pow(Math.Cos(execute f), execute p)
            | Int(x) -> float(x)
            | Fact(x) -> fact(execute x)
            | Float(x) -> x
            | Constant(Pi) -> Math.PI
            | Constant(E) -> Math.E
            | Neg(e) -> -1. * execute(e)
            | Integral(func, diffVar, lowBE, uppBE) -> 
                let diffParameter = Expression.Parameter(typeof<float>, diffVar)
                let expr = buildExpression diffParameter func
                let lam = Expression.Lambda<Func<float, float>>(expr, diffParameter)
                let lamFunc = lam.Compile()
                Integrate.OnClosedInterval(lamFunc, execute(lowBE), execute(uppBE))
            | DsAst(SRefValue(dsAst)) -> interpretSValue dsAst
            | Sum(exprs) -> expand exprs [] |> List.fold (fun s i -> s + i) 0.
            | Prod(exprs) -> expand exprs [] |>  List.fold (fun s i -> s * i) 1.
            | any -> failwithf "Cannot evaluate next AST: \r\n %A" any
    
    and expand exprs state =
        match exprs with
            | [] -> List.rev state
            | DsAst(SRefValue(dsAst))::t -> expand t (interpretSValue dsAst::state)
            | DsAst(MRefValue(dsAst))::t -> List.fold (fun s ast -> interpretMValue ast @ s) state dsAst |> expand t
            | any::t -> expand t ((execute any)::state)
                
    and extractExpressionValues lamParameter ast1 ast2 = 
        let e1 = buildExpression lamParameter ast1
        let e2 = buildExpression lamParameter ast2
        (e1, e2)
    
    and buildExpression lamParameter expr : Expression = 
        match expr with
            | Plus(e1, e2) -> 
                let v1, v2 = extractExpressionValues lamParameter e1 e2
                Expression.Add(v1, v2) :> Expression
            | Sub(e1, e2) -> 
                let v1, v2 = extractExpressionValues lamParameter e1 e2
                Expression.Subtract(v1, v2) :> Expression
            | Mul(e1, e2) -> 
                let v1, v2 = extractExpressionValues lamParameter e1 e2
                Expression.Multiply(v1, v2) :> Expression
            | Div(e1, e2) -> 
                let v1, v2 = extractExpressionValues lamParameter e1 e2
                Expression.Divide(v1, v2) :> Expression
            | Var(x) -> 
                if x <> lamParameter.Name then failwithf "Incorrect parameter. Expected: '%s'. Actual: '%s'" lamParameter.Name x
                else lamParameter :> Expression
            | Pow(e1, e2) -> 
                buildPower (buildExpression lamParameter e1) (buildExpression lamParameter e2) :> Expression
            | Sin(f, p) -> 
                buildSin (buildExpression lamParameter f) |> buildPower <| buildExpression lamParameter p :> Expression
            | Cos(f, p) -> 
                let v1, v2 = extractExpressionValues lamParameter f p
                let par = <@1.@>
                ExpressionBuilder.buildExpression <@ Math.Pow(Math.Sin(%par), 2. ) @>
                buildCos (buildExpression lamParameter f) |> buildPower <| buildExpression lamParameter p :> Expression
            | Fact(x) -> 
                let xVal = execute x
                ExpressionBuilder.buildExpression <@ MathFunctions.fact xVal @>
            | expr -> Expression.Constant(execute expr) :> Expression

    member this.Eval ast = execute ast
    
