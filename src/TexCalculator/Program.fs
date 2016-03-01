module Program

open System
open TexParser
open CodeGeneration
open DsParser
open Microsoft.FSharp.Text.Lexing
open Compiler
open DsParserExtensions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System.Linq.Expressions
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
    let stringFormula = "\int_{0}^{\pi/2}{[|for A with Id = \"A2\" select IntProp|] * \sin{x}} d{x}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
//    let stringFormula = "\int_{0}^{\pi}{\sin{x} d{x}} + \int_{0}^{\\frac{\pi}{2}}{\cos{x} d{x}}"
//    let stringFormula = "2*\sin{9 \\bmod (1*5+4)"
//    let stringFormula = "1+{2}^{3}+\sum{1, 2, 3}"
//    let stringFormula = "[|     |]"
    let lexbuf = LexBuffer<char>.FromString stringFormula
    let lexems = readLexems lexbuf
    let lexBuf = LexBuffer<char>.FromString stringFormula
    let l = TexParser._fsyacc_stateToProdIdxsTableElements
    let compiler = TexCompiler(DsParser.parse, DomainSpecificInterpreter.evalSValue, DomainSpecificInterpreter.evalMValue)
    let intermediateExpression = 
        compiler.Compile stringFormula
        |> QuotationEvaluator.ToLinqExpression :?> Expression<Func<Unit, float>>
    let result = intermediateExpression.Compile().Invoke()
    printfn "%A" result
    0


