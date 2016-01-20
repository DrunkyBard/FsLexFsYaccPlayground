module TexAst

type Constant = 
    | Pi
    | E

type Expr = 
    | Plus of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Sum of Expr list
    | Prod of Expr list
    | Fact of Expr
    | Pow of Expr*Expr
    | Int of int
    | Var of string
    | Diff of string
    | Constant of Constant
    | Integral of Expr*string*Expr*Expr // function x differentialVariable x lowerBound x upperBound
    | Lim of Expr*string*Expr  // function x limitVariable x approacheValue
    | Neg of Expr
