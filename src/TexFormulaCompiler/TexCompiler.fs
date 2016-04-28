module Compiler

open TexAst
open System
open Microsoft.FSharp.Text.Lexing
open CodeGeneration
open ErrorHandling


type TexCompiler(domainSpecificAnalyzer: DomainSpecificContext -> obj, dslSValueInterpreter, dslMValueInterpreter) = 
    member this.Compile src = 
        let errorLogger = ErrorLogger(fun s -> Console.Error.WriteLine(s))
        TexParser.domainSpecificAnalyzer <- domainSpecificAnalyzer
        TexLexer.errorLogger <- errorLogger
        TexParser.errorLogger <- errorLogger
        let lexbuf = LexBuffer<char>.FromString src
        let texCodeGen = TexCodeGenerator(dslSValueInterpreter, dslMValueInterpreter)
        let ast = (TexLexer.lex, lexbuf) ||> TexParser.start

        if errorLogger.HasErrors then 
            errorLogger.Flush()
            None
        else Some(ast |> texCodeGen.Generate)
