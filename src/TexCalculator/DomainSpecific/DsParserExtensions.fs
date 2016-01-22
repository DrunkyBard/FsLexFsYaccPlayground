module DsParserExtensions

open Microsoft.FSharp.Text.Lexing
open TexAst

let parse (token: DomainSpecificToken) = 
    let lexbuf src = LexBuffer<char>.FromString src
    let parse single src = (DsLex.lex single, lexbuf src) ||> DsParser.start :> obj

    match token with
        | SRefValueSrc(src) -> parse true src
        | MRefValueSrc(src) -> parse false src
