open Microsoft.FSharp.Text.Lexing

[<EntryPoint>]
let main argv = 
    let sqlQuery = "
        SELECT a.Id, b.Name
        FROM Table1 AS a
        INNER JOIN Table2 b ON a.Id = b.Id
        WHERE a.Name = 123 AND b.SomeCol = true
        ORDER BY a.Name ASC, b.Id DESC
    "

    let qw = "SELECT b.a FROM b"
    let lexbuf = LexBuffer<char>.FromString qw
//    let a = SqlLexer.lex lexbuf
//    let b = SqlLexer.lex lexbuf
//    let oo = SqlLexer.lex lexbuf
    let ast = SqlParser.start SqlLexer.lex lexbuf

    printfn "%A" argv
    0 
