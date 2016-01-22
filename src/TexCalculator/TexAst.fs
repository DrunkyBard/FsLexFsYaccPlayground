module TexAst

exception DomainSpecificParseException of string * int

type Constant = 
    | Pi
    | E

type DomainSpecificToken = 
    | SRefValueSrc of string
    | MRefValueSrc of string

type DomainSpecificAst =
    | SRefValue of obj
    | MRefValue of obj list

type Expr = 
    | Plus of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Sum of string option*string option*(Expr list)  // idxIdentifier x upperBoundIdentifier x args
    | Prod of string option*string option*(Expr list) // idxIdentifier x upperBoundIdentifier x args
    | Fact of Expr
    | Pow of Expr*Expr
    | Int of int
    | Var of string
    | Diff of string
    | Constant of Constant
    | Integral of Expr*string*Expr*Expr // function x differentialVariable x lowerBound x upperBound
    | Lim of Expr*string*Expr  // function x limitVariable x approacheValue
    | DsAst of DomainSpecificAst
    | Neg of Expr

type DomainSpecificAnalyzer = 
    abstract member Analyze: DomainSpecificToken -> DomainSpecificAst 

type SpecAnalyzer() = 
    interface DomainSpecificAnalyzer with
        member this.Analyze src = 
            SRefValue(1)
