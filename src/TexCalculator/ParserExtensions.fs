module ParserExtensions

open TexAst

module TexParser = 
    let parse (domainSpecificAnalyzer : DomainSpecificToken -> DomainSpecificAst) lexbuf = 
        TexParser.domainSpecificAnalyzer <- domainSpecificAnalyzer
        (TexLexer.lex, lexbuf) ||> TexParser.start
