namespace ErrorHandling

type Position = { Line: int; Column: int }

type ParseError = Error of string * Position

type ErrorLogger(out : string -> unit) = 
    let mutable errors = []

    member this.ReportError (Error(message, pos)) = 
        let errorMsg = sprintf "Error occured at Line: %d, Column: %d : %s" pos.Line pos.Column message
        errors <- (errorMsg, pos)::errors
    
    member this.Flush() = 
        Seq.sortBy (fun err -> (snd err).Line, (snd err).Column) errors
        |> Seq.iter(fun err -> out(fst err))

    member this.HasErrors with get() = (Seq.length errors) > 0