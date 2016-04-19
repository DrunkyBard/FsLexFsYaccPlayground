module Program

open System
open TexParser
open DsParser
open Microsoft.FSharp.Text.Lexing
open Compiler
open DsParserExtensions
open System.Linq.Expressions
open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing


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

[<EntryPoint>]
let main argv = 
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = "\sin{{2}^{3}}}"
    let stringFormula = "\sin{2+7}"
    let stringFormula = "sum{2+7}"
    let stringFormula = "sin{2+7}"
    let stringFormula = @"sum {{2}^{2}, 2, 3}"
//    let stringFormula = "\int_{2}^{3}{x + 2}d{x}"
//    let stringFormula = "{2}^{3}}}}}}"
    let lexbuf = LexBuffer<char>.FromString stringFormula
//    lexbuf.StartPos.Line
//    lexbuf.StartPos.Column
//    lexbuf.Lexeme.ToString()
    let lexems = readLexems lexbuf
    let compiler = TexCompiler(DsParser.parse, DomainSpecificInterpreter.evalSValue, DomainSpecificInterpreter.evalMValue)
    let intermediateExpression = 
        compiler.Compile stringFormula
        |> QuotationEvaluator.ToLinqExpression :?> Expression<Func<Unit, float>>
    let result = intermediateExpression.Compile().Invoke()
    printfn "%A" result
    Console.ReadLine()
    0


