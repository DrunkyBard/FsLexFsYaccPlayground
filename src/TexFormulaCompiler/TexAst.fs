﻿module TexAst

open Microsoft.FSharp.Reflection

exception DomainSpecificParseException of string * int

type Constant = 
    | Pi
    | E

type DomainSpecificContext = 
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
    | Bmod of Expr * Expr
    | Sum of Expr list  
    | Prod of Expr list 
    | Sqrt of Expr * Expr // func * power
    | Sin of Expr * Expr // func * power
    | Cos of Expr * Expr // func * power
    | Fact of Expr
    | Pow of Expr * Expr
    | Int of int
    | Float of float
    | Var of string
    | Constant of Constant
    | Integral of Expr * string * Expr * Expr // function x differentialVariable x lowerBound x upperBound
    | Lim of Expr * string * Expr  // function x limitVariable x approximationValue
    | DsAst of DomainSpecificAst
    | Neg of Expr

let toString(t: 'a) = let case, _ = FSharpValue.GetUnionFields(t, typeof<'a>) in case.Name

type TokenInternal = 
    | FRAC
    | SQRT
    | SUM
    | PROD
    | INTEGRAL
    | SIN
    | COS
    | POW
    | BINARY
    override this.ToString() = toString(this)

type ParenToken = 
    | LPAREN
    | RPAREN
    | LCURLY
    | RCURLY
    override this.ToString() = toString(this)
