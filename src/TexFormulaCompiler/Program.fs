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
open Microsoft.FSharp.Reflection


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

type Prov = FSharp.Configuration.ResXProvider<file="Resources/ErrorMsg.resx">

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

[<EntryPoint>]
let main argv = 
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = "\sin{{2}^{3}}}"
    let stringFormula = "\sin{2+7}"
    let stringFormula = "sin{2+7}"
    let stringFormula = @"sum {{2}^{2}, 2 ) 3}}"

    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = @"sum {{2}^{2}, 2 ) 3}}"
    let stringFormula = @"\frac{1}{2}"
    let stringFormula = @"{2}2}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + 432 1+2!, 3!+4)*3)}}^{3!} * 5"
    let stringFormula = @"(1+2)"
    let stringFormula = @"\frac{1}{2}^{3}"
    let stringFormula = @"((1+2)*3)"
    let stringFormula = @"{2}^{3}"
    let stringFormula = @"\int_{1}^{2}{x}d{x}"
    let stringFormula = "1*2+5 * {\\frac{\int_{2}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = @"{2{3}"
    let stringFormula = @"\frac{1}{2}}3}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let stringFormula = @"\sum{1}"
    let stringFormula = @"{\frac{ \int_{2}^{\pi}{x}d{x} }{ ( (4+\sum{1+2!, 3!+4})*3 ) }}^{2}*5"
    let stringFormula = "1+2 3^2"
    let stringFormula = "sum{2+7}"
    let stringFormula = @"sum {{2}^{2}, 2 ) 3}}"
    let stringFormula = @"{2}\{3}"
    let stringFormula = @"\\{1}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|for A with Id = \"A2\" select IntProp|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
    let a = toString(TexAst.TokenInternal.BINARY)
//    let stringFormula = @"\frac{1}{2}"
//    let stringFormula = @"abc {{2}^{2}, 2 ) 3}}"
//    let stringFormula = @"abc {1}"
//    let stringFormula = "\int_{2}^{3}{x + 2}d{x}"
//    let stringFormula = "{2}^{3}}}}}}"
    let lexbuf = LexBuffer<char>.FromString stringFormula
//    lexbuf.StartPos.Line
//    lexbuf.StartPos.Column
//    lexbuf.Lexeme.ToString()
    let lexems = readLexems lexbuf
    let compiler = TexCompiler(DsParser.parse, DomainSpecificInterpreter.evalSValue, DomainSpecificInterpreter.evalMValue)
    match compiler.Compile stringFormula with
        | Some(expr) -> 
                        let intermediateExpr = expr |> QuotationEvaluator.ToLinqExpression :?> Expression<Func<Unit, float>>
                        intermediateExpr.Compile().Invoke() |> printfn "%A"
        | None -> ()
    
    printfn "%s" stringFormula
    Console.ReadLine()
    0


