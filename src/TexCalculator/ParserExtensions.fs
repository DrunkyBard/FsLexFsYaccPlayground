module ParserExtensions

open TexAst

module TexParser = 
    let parse (domainSpecificAnalyzer : DomainSpecificToken -> obj) lexbuf = 
        TexParser.domainSpecificAnalyzer <- domainSpecificAnalyzer
        (TexLexer.lex, lexbuf) ||> TexParser.start
