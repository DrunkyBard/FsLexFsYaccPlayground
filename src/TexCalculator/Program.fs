module Program
open TexParser
open TexInterpreter
open TexAst
open DsLex
open DsParser
open Microsoft.FSharp.Text.Lexing
open ParserExtensions
open System.Linq.Expressions

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

//    let stringFormula = "for human select voice_freq where name=\"Lex\" and gender=1 or name=\"Parse\" and gender = 2"
//    let lexBuf = LexBuffer<char>.FromString stringFormula
//    let lexems = readLexems1 false lexBuf
//    let ast1 = (DsLex.lex false, lexBuf) ||> DsParser.start
      
//    let stringFormula = "(4 + \sum{1+2!, 3!+4}) + 5"
//    let stringFormula = "1*2+3*4!/3"
//    let stringFormula = "1!+2+1"
//    let stringFormula = "1+5*4"
//    let stringFormula = "1*2+3*4"
//    let stringFormula = "{1}^{{2}^{3}}"
//    let stringFormula = "1*2+5* {\\frac{\int_{[||for human select voice_freq where name=\"Lex\" and gender=1 or name=\"Parse\" and gender = 2||]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
//    let stringFormula = "[|for human select voice_freq where name=\"Lex\" and gender=1 or name=\"Parse\" and gender = 2|]"
//    let stringFormula = "\\frac{\int_{0}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}"
//    let stringFormula = "2+3/4"
//    let stringFormula = "1*2+\sum_{i}^{N}{N*\\text{[||for human select voice_freq where name=\"Lex\" and gender=1 or name=\"Parse\" and gender = 2||]}, 2, 5*2}"
//    let stringFormula = "1+2"
    let stringFormula = "\int_{0}^{10}{x+10} d{x}"
    let lexBuf = LexBuffer<char>.FromString stringFormula
//    let lexems = readLexems  lexBuf 
//    let par = Expression.Parameter(typeof<int>, "x")
//    let lam = Expression.Lambda<System.Func<int, int>>(
//        Expression.Add(par, Expression.Constant(2)), par).Compile()
//    let w = lam.Invoke(5)
    let ast = (DsParserExtensions.parse, lexBuf) ||> TexParser.parse
    let res = TexInterpreter.execute ast
    printfn "%A" ast
    0



