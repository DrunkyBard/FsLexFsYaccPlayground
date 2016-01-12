open Microsoft.FSharp.Text.Lexing
open SqlParser

let readLexems lexbuf =
    let rec readLexemsInternal state = function
        | EOF -> EOF::state
        | any -> readLexemsInternal (any::state) (SqlLexer.lex lexbuf)
    
    readLexemsInternal [] (SqlLexer.lex lexbuf) |> List.rev

[<EntryPoint>]
let main argv = 
    let sqlQuery = "
        SELECT a.Id, b.Name
        FROM Table1 AS a
        INNER JOIN Table2 b ON a.Id = b.Id
        WHERE a.Name = 123 AND b.SomeCol = true
        ORDER BY a.Name ASC, b.Id DESC
    "

    let qw = "
        SELECT a.Id, b.Name
        FROM Table1 AS a
        INNER JOIN Table2 b ON a.Id = b.Id
        WHERE a.Name = 123 AND b.SomeCol = true
        ORDER BY a.Name ASC, b.Id DESC
    "
    let lexbuf = LexBuffer<char>.FromString qw
//    let lexs = readLexems lexbuf
//    let a = SqlLexer.lex lexbuf
//    let b = SqlLexer.lex lexbuf
//    let oo = SqlLexer.lex lexbuf
    let ast = SqlParser.start SqlLexer.lex lexbuf

    printfn "%A" ast
    0 
