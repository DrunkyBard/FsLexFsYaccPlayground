module Program
open TexParser
open Microsoft.FSharp.Text.Lexing

let readLexems lexbuf =
    let rec readLexemsInternal state = function
        | EOF -> EOF::state
        | any -> readLexemsInternal (any::state) (TexLexer.lex lexbuf)
    
    readLexemsInternal [] (TexLexer.lex lexbuf) |> List.rev

[<EntryPoint>]
let main argv = 
//    let stringFormula = "(4 + \sum{1+2!, 3!+4}) + 5"
//    let stringFormula = "1*2+3*4!/3"
//    let stringFormula = "1!+2+1"
//    let stringFormula = "1+5*4"
//    let stringFormula = "1*2+3*4"
//    let stringFormula = "{1}^{{2}^{3}}"
    let stringFormula = "1*2+5* {\\frac{\int_{[|Some domain specific data|]}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}}^{3!} * 5"
//    let stringFormula = "\\frac{\int_{0}^{\pi} {x} d{x}}{((4 + \sum{1+2!, 3!+4})*3)}"
//    let stringFormula = "2+3/4"
    let lexBuf = LexBuffer<char>.FromString stringFormula

    let lexems = readLexems lexBuf
    let ast = (TexLexer.lex, lexBuf) ||> TexParser.start
    printfn "%A" ast
    0



