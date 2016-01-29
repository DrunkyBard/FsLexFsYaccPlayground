﻿module Program

open System
open TexParser
open TexInterpreter
open DsParser
open Microsoft.FSharp.Text.Lexing
open ParserExtensions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Evaluator


let readLexems lexbuf =
    let rec readLexemsInternal state = function
        | TexParser.token.EOF -> TexParser.token.EOF::state
        | any -> readLexemsInternal (any::state) (TexLexer.lex lexbuf)
    
    readLexemsInternal [] (TexLexer.lex lexbuf) |> List.rev
    
let readLexems1 single lexbuf =
    let rec readLexemsInternal state = function
        | DsParser.token.EOF -> DsParser.token.EOF::state
        | any -> readLexemsInternal (any::state) (DsLex.lex single lexbuf)
    
    readLexemsInternal [] (DsLex.lex single lexbuf) |> List.rev

let visitQuote q =
    match q with
        | Call(exprOpt, mi, exprList) -> printf "call" 

[<EntryPoint>]
let main argv = 
//    let lamParam = Quotations.Var("AAA", typeof<float>) |> Quotations.Expr.Var :?> Quotations.Expr<float>
//    let xParamName = "x"
//    let varX = Quotations.Var("xParam", typeof<int>)
//    let varXExpr = Expr.Cast<int>(Quotations.Expr.Var(varX))
//    let lamBody = <@ %varXExpr + 1 @>
//    let lam = Expr.Cast<int -> int>(Expr.Lambda(varX, lamBody))
//    let compiledLam = QuotationEvaluator.Evaluate lam
//    Expr.Lambda(varX, )
    
    
//
//    match lam with
//        | Lambda(param, body) -> printf "abc"
    let q1 = <@ 2.0 @>
    let q2 = <@ 3.0 @>
    let q3 = <@ 4.0 @>
    
    let stringFormula = "\int_{0}^{\pi/2}{[|for A with Id = \"A2\" select IntProp|] * \sin{x}} d{x}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = "5*\sum{[||for A select IntProp where Id < \"Z\"||]}"
    let lexbuf = LexBuffer<char>.FromString stringFormula
    let lexems = readLexems lexbuf
    let lexBuf = LexBuffer<char>.FromString stringFormula
    let ast = (DsParserExtensions.parse, lexBuf) ||> TexParser.parse
    let res = TexInterpreter(DomainSpecificInterpreter.evalSValue, DomainSpecificInterpreter.evalMValue).Eval ast
    printfn "%A" ast
    0



