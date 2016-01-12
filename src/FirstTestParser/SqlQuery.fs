module SqlQuery

type Column = 
    { 
        TableRef : string
        ColumnName : string 
    }

type Table = 
    {
        Alias  : string option
        Name : string
    }

type Op = Eq | Neq | Lt | Gt | Leq | Geq 

type Value = 
    | Int of int
    | Bool of bool
    | String of string

type Expr = 
    | RefOnly of Op * Column * Column
    | RefValue of Op * Column * Value
    | Value of Op * Value * Value

type WhereClause = 
    | Expr of Expr
    | And of WhereClause * WhereClause
    | Or of WhereClause * WhereClause

type JoinType = INNER | LEFT | RIGHT

type JoinClause = Table * JoinType * WhereClause option

type OrderDirection = ASC | DESC

type OrderClause = OrderDirection*Column

type SqlQuery = 
    { 
        Table: Table
        Columns: Column list
        Joins: JoinClause list
        Where: WhereClause option
        Order: OrderClause list
    }

type NopType = int
