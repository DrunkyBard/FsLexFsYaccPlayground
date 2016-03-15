module DsParserExtensions

open Microsoft.FSharp.Text.Lexing
open TexAst

module DsParser = 
    let parse (ctx: DomainSpecificContext) = 
        let lexbuf src = LexBuffer<char>.FromString src
        let parse single src = (DsLex.lex single, lexbuf src) ||> DsParser.start :> obj
    
        match ctx with
            | SRefValueSrc(src) -> parse true src
            | MRefValueSrc(src) -> parse false src
