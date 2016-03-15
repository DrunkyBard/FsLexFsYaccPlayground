module Compiler

open TexAst
open Microsoft.FSharp.Text.Lexing
open CodeGeneration


type TexCompiler(domainSpecificAnalyzer: DomainSpecificContext -> obj, dslSValueInterpreter, dslMValueInterpreter) = 
    member this.Compile src = 
        TexParser.domainSpecificAnalyzer <- domainSpecificAnalyzer
        let lexbuf = LexBuffer<char>.FromString src
        let texCodeGen = TexCodeGenerator(dslSValueInterpreter, dslMValueInterpreter)

        (TexLexer.lex, lexbuf) 
        ||> TexParser.start
        |> texCodeGen.Generate
