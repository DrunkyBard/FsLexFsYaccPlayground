module QuotationExtensions

open Microsoft.FSharp.Quotations

type Expr with
    static member Var<'T> (variable: Quotations.Var) = Expr.Cast<'T>(Expr.Var(variable))
    
