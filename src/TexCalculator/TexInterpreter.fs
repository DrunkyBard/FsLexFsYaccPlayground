module TexInterpreter

open System
open TexAst
open MathNet.Numerics
open MathFunctions
open FSharp.Quotations.Evaluator
open QuotationExtensions

type TexInterpreter (interpretSValue: obj -> float, interpretMValue: obj -> float list)=
    let interpretSValue = interpretSValue
    let interpretMValue = interpretMValue

    let rec execute (ast: TexAst.Expr) (funcArguments: Quotations.Var list) : Quotations.Expr<float> =
        match ast with
            | Plus(e1, e2) -> let expr1, expr2 = flattenBinaryExpression e1 e2 funcArguments in <@ %expr1 + %expr2 @>
            | Mul (e1, e2) -> let expr1, expr2 = flattenBinaryExpression e1 e2 funcArguments in <@ %expr1 * %expr2 @>
            | Sub (e1, e2) -> let expr1, expr2 = flattenBinaryExpression e1 e2 funcArguments in <@ %expr1 - %expr2 @>
            | Div (e1, e2) -> let expr1, expr2 = flattenBinaryExpression e1 e2 funcArguments in <@ %expr1 / %expr2 @>
            | Pow (e1, e2) -> let expr1, expr2 = flattenBinaryExpression e1 e2 funcArguments in <@ Math.Pow(%expr1, %expr2) @>
            | Sqrt (e, powE) -> let expr1, expr2 = flattenBinaryExpression e powE funcArguments in <@ Math.Pow(%expr1, 1./(%expr2)) @>
            | Sin(f, p) -> let expr1, expr2 = flattenBinaryExpression f p funcArguments in <@ Math.Pow(Math.Sin(%expr1), 1./(%expr2)) @>
            | Cos(f, p) -> let expr1, expr2 = flattenBinaryExpression f p funcArguments in <@ Math.Pow(Math.Cos(%expr1), 1./(%expr2)) @>
            | Int(x) -> let floatX = float(x) in <@ floatX @>
            | Float(x) -> <@ x @>
            | Fact(x) -> let expr = execute x funcArguments in <@ MathFunctions.fact(%expr) @>
            | Constant(Pi) -> <@ Math.PI @>
            | Constant(E) -> <@ Math.E @>
            | Neg(e) -> let expr = execute e funcArguments in <@ -1. * %expr @>
            | DsAst(SRefValue(dsAst)) -> let sValue = interpretSValue dsAst in <@ sValue @>
            | Sum(exprs) ->  let exprList = expand exprs funcArguments [] in <@ Array.fold (fun s i -> s + i) 0. %exprList @>
            | Prod(exprs) -> let exprList = expand exprs funcArguments [] in <@ Array.fold (fun s i -> s * i) 1. %exprList @>
            | Var(varName) -> 
                match List.tryFind (fun (arg: Quotations.Var) -> arg.Name = varName) funcArguments with
                    | Some(arg) -> <@ %Quotations.Expr.Var<float>(arg) @>
                    | None -> failwithf "Parameter \'%s\' is not defined" varName
            | Integral(func, diffVar, lowBE, uppBE) -> 
                let lamParam = Quotations.Var(diffVar, typeof<float>)
                let intExpr = execute func (lamParam::funcArguments)
                let lambda = Quotations.Expr.Cast<float -> float>(Quotations.Expr.Lambda(lamParam, intExpr))
                let compiledQuot =  QuotationEvaluator.Evaluate lambda
                let lowBEExpression = execute lowBE funcArguments
                let uppBEExpression = execute uppBE funcArguments
                <@ Integrate.OnClosedInterval((fun x -> compiledQuot x), %lowBEExpression, %uppBEExpression) @>
            | any -> failwithf "Cannot evaluate next AST: \r\n %A" any

    and flattenBinaryExpression e1 e2 funcArguments = 
        let e1 = execute e1 funcArguments
        let e2 = execute e2 funcArguments
        (e1, e2)

    and expand exprs funcArguments state =
        match exprs with
            | [] -> let revState = List.rev state in Quotations.Expr.Cast<float array>(Quotations.Expr.NewArray(typeof<float>, revState))
            | DsAst(MRefValue(dsAst))::t -> 
                List.collect (fun dAst -> interpretMValue dAst) dsAst
                |> List.map (fun value -> <@ value @> :> Quotations.Expr)
                |> List.append state
                |> expand t funcArguments
            | h::t -> expand t funcArguments ((execute h funcArguments :> Quotations.Expr)::state)

    member this.Eval ast = execute ast [] |> QuotationEvaluator.Evaluate
    
