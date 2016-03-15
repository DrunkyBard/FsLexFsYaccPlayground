module QueryStatement

type Value = 
    | Int of int
    | String of string

type SingleStatement = { Identity: Value }

type ManyStatement = { Where: string }

type Op = 
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge

type WhereStatement = 
    | Operation of Op * string * Value
    | And of WhereStatement * WhereStatement
    | Or of WhereStatement * WhereStatement

type StatementType = 
    | Single of string * Value
    | Many of WhereStatement

type Statement = 
    {
        For: string
        Select: string
        Type: StatementType
    }

