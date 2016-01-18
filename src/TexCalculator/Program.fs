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
    let stringFormula = "4 + \sum{1+2, 3+4} + 5"
//    let stringFormula = "1 + 5"
    let lexBuf = LexBuffer<char>.FromString stringFormula
//    let lexems = readLexems lexBuf
    let ast = (TexLexer.lex, lexBuf) ||> TexParser.start
    printfn "%A" argv
    0
