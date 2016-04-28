namespace ErrorHandling

open System
open TexAst

type ResX = FSharp.Configuration.ResXProvider<file="Resources/ErrorMsg.resx">

type ErrorTextFormatter = 
    static member FunctionHasNoParameters token = 
        match token with
            | SUM | PROD -> String.Format(ResX.ErrorMsg.FunctionShouldHaveAtLeastOneParameter, token.ToString())
            | _ -> String.Format(ResX.ErrorMsg.FunctionShouldHaveOnlyOneParameter, token.ToString())

    static member FunctionHasSeveralParameters token = String.Format(ResX.ErrorMsg.SeveralArgumentsDetected, token.ToString())

    static member UnmatchedLeftBracket lBracket = String.Format(ResX.ErrorMsg.UnmatchedLBracket, lBracket.ToString())

    static member UnmatchedRightBracket rBracket = String.Format(ResX.ErrorMsg.UnmatchedRBracket, rBracket.ToString())

    static member FunctionHasNoSlashSymbol functionToken = String.Format(ResX.ErrorMsg.MissingSlash, functionToken.ToString())

    static member MissingCaretSymbol = ResX.ErrorMsg.MissingCaret
    
    static member MissingUnderscoreSymbol = ResX.ErrorMsg.MissingUnderscore
    
    static member FracMissingSecondArg = ResX.ErrorMsg.FracMissingSecondArg

    static member MissingDiffSyntax = ResX.ErrorMsg.MissingDiff

    static member MissingFirstOperand = ResX.ErrorMsg.MissingFirstOp

    static member MissingSecondOperand = ResX.ErrorMsg.MissingSecondOp

    static member MaybeMissingBinaryOperator = ResX.ErrorMsg.MaybeMissingOps

    static member UnexpectedFunctionName (funcName: string) = String.Format(ResX.ErrorMsg.UnexpectedFunc, funcName)

    static member InvalidLRBracketExpression = ResX.ErrorMsg.InvalidLRBracketExpr

    static member UnexpectedSlash = ResX.ErrorMsg.UnexpectedSlash

    static member UnexpectedSymbol (symbol: string) = String.Format(ResX.ErrorMsg.UnexpectedSymbol, symbol)

    static member EmptyRef = ResX.ErrorMsg.EmptyRef

    static member UnexpectedRefComma = ResX.ErrorMsg.UnexpectedRefComma