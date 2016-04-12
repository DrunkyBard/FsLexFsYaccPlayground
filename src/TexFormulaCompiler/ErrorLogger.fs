namespace ErrorHandling

type Position = { Line: int; Column: int }

type ParseError = Error of string * Position

type ErrorLogger(out : string -> unit) = 
    member this.ReportError (Error(message, pos)) = 
        let pos2 : Position = {Line = 2; Column = 3}
        let errorMsg = sprintf "%s at Line: %d, Column: %d" message pos.Line pos.Column
        out(errorMsg)