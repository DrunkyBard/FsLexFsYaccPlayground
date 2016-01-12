// Implementation file for parser generated by fsyacc
module SqlParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open SqlQuery
open Microsoft.FSharp.Text.Lexing

# 11 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | DOT
  | COMMA
  | EQ
  | LT
  | LE
  | GT
  | GE
  | NEQ
  | ASC
  | DESC
  | AND
  | OR
  | ON
  | JOIN
  | INNER
  | LEFT
  | RIGHT
  | SELECT
  | FROM
  | AS
  | WHERE
  | ORDER
  | BY
  | ID of (string)
  | BOOL of (bool)
  | STRING of (string)
  | INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_DOT
    | TOKEN_COMMA
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_LE
    | TOKEN_GT
    | TOKEN_GE
    | TOKEN_NEQ
    | TOKEN_ASC
    | TOKEN_DESC
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_ON
    | TOKEN_JOIN
    | TOKEN_INNER
    | TOKEN_LEFT
    | TOKEN_RIGHT
    | TOKEN_SELECT
    | TOKEN_FROM
    | TOKEN_AS
    | TOKEN_WHERE
    | TOKEN_ORDER
    | TOKEN_BY
    | TOKEN_ID
    | TOKEN_BOOL
    | TOKEN_STRING
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_fromRule
    | NONTERM_columns
    | NONTERM_column
    | NONTERM_joinRuleList
    | NONTERM_joinRule
    | NONTERM_joinTable
    | NONTERM_joinType
    | NONTERM_joinWhereExpr
    | NONTERM_whereRule
    | NONTERM_whereExpr
    | NONTERM_whereOp
    | NONTERM_orderRule
    | NONTERM_orderDirectionRule
    | NONTERM_value
    | NONTERM_op

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | DOT  -> 1 
  | COMMA  -> 2 
  | EQ  -> 3 
  | LT  -> 4 
  | LE  -> 5 
  | GT  -> 6 
  | GE  -> 7 
  | NEQ  -> 8 
  | ASC  -> 9 
  | DESC  -> 10 
  | AND  -> 11 
  | OR  -> 12 
  | ON  -> 13 
  | JOIN  -> 14 
  | INNER  -> 15 
  | LEFT  -> 16 
  | RIGHT  -> 17 
  | SELECT  -> 18 
  | FROM  -> 19 
  | AS  -> 20 
  | WHERE  -> 21 
  | ORDER  -> 22 
  | BY  -> 23 
  | ID _ -> 24 
  | BOOL _ -> 25 
  | STRING _ -> 26 
  | INT _ -> 27 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_DOT 
  | 2 -> TOKEN_COMMA 
  | 3 -> TOKEN_EQ 
  | 4 -> TOKEN_LT 
  | 5 -> TOKEN_LE 
  | 6 -> TOKEN_GT 
  | 7 -> TOKEN_GE 
  | 8 -> TOKEN_NEQ 
  | 9 -> TOKEN_ASC 
  | 10 -> TOKEN_DESC 
  | 11 -> TOKEN_AND 
  | 12 -> TOKEN_OR 
  | 13 -> TOKEN_ON 
  | 14 -> TOKEN_JOIN 
  | 15 -> TOKEN_INNER 
  | 16 -> TOKEN_LEFT 
  | 17 -> TOKEN_RIGHT 
  | 18 -> TOKEN_SELECT 
  | 19 -> TOKEN_FROM 
  | 20 -> TOKEN_AS 
  | 21 -> TOKEN_WHERE 
  | 22 -> TOKEN_ORDER 
  | 23 -> TOKEN_BY 
  | 24 -> TOKEN_ID 
  | 25 -> TOKEN_BOOL 
  | 26 -> TOKEN_STRING 
  | 27 -> TOKEN_INT 
  | 30 -> TOKEN_end_of_input
  | 28 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_fromRule 
    | 3 -> NONTERM_columns 
    | 4 -> NONTERM_columns 
    | 5 -> NONTERM_column 
    | 6 -> NONTERM_column 
    | 7 -> NONTERM_column 
    | 8 -> NONTERM_joinRuleList 
    | 9 -> NONTERM_joinRuleList 
    | 10 -> NONTERM_joinRuleList 
    | 11 -> NONTERM_joinRule 
    | 12 -> NONTERM_joinTable 
    | 13 -> NONTERM_joinType 
    | 14 -> NONTERM_joinType 
    | 15 -> NONTERM_joinType 
    | 16 -> NONTERM_joinWhereExpr 
    | 17 -> NONTERM_joinWhereExpr 
    | 18 -> NONTERM_whereRule 
    | 19 -> NONTERM_whereRule 
    | 20 -> NONTERM_whereExpr 
    | 21 -> NONTERM_whereExpr 
    | 22 -> NONTERM_whereExpr 
    | 23 -> NONTERM_whereOp 
    | 24 -> NONTERM_whereOp 
    | 25 -> NONTERM_whereOp 
    | 26 -> NONTERM_orderRule 
    | 27 -> NONTERM_orderRule 
    | 28 -> NONTERM_orderDirectionRule 
    | 29 -> NONTERM_orderDirectionRule 
    | 30 -> NONTERM_orderDirectionRule 
    | 31 -> NONTERM_value 
    | 32 -> NONTERM_value 
    | 33 -> NONTERM_value 
    | 34 -> NONTERM_op 
    | 35 -> NONTERM_op 
    | 36 -> NONTERM_op 
    | 37 -> NONTERM_op 
    | 38 -> NONTERM_op 
    | 39 -> NONTERM_op 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 30 
let _fsyacc_tagOfErrorTerminal = 28

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | DOT  -> "DOT" 
  | COMMA  -> "COMMA" 
  | EQ  -> "EQ" 
  | LT  -> "LT" 
  | LE  -> "LE" 
  | GT  -> "GT" 
  | GE  -> "GE" 
  | NEQ  -> "NEQ" 
  | ASC  -> "ASC" 
  | DESC  -> "DESC" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | ON  -> "ON" 
  | JOIN  -> "JOIN" 
  | INNER  -> "INNER" 
  | LEFT  -> "LEFT" 
  | RIGHT  -> "RIGHT" 
  | SELECT  -> "SELECT" 
  | FROM  -> "FROM" 
  | AS  -> "AS" 
  | WHERE  -> "WHERE" 
  | ORDER  -> "ORDER" 
  | BY  -> "BY" 
  | ID _ -> "ID" 
  | BOOL _ -> "BOOL" 
  | STRING _ -> "STRING" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | NEQ  -> (null : System.Object) 
  | ASC  -> (null : System.Object) 
  | DESC  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | ON  -> (null : System.Object) 
  | JOIN  -> (null : System.Object) 
  | INNER  -> (null : System.Object) 
  | LEFT  -> (null : System.Object) 
  | RIGHT  -> (null : System.Object) 
  | SELECT  -> (null : System.Object) 
  | FROM  -> (null : System.Object) 
  | AS  -> (null : System.Object) 
  | WHERE  -> (null : System.Object) 
  | ORDER  -> (null : System.Object) 
  | BY  -> (null : System.Object) 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 3us; 4us; 1us; 65535us; 2us; 3us; 8us; 65535us; 2us; 13us; 11us; 12us; 31us; 40us; 33us; 40us; 36us; 40us; 38us; 40us; 41us; 43us; 48us; 49us; 1us; 65535us; 4us; 5us; 2us; 65535us; 4us; 18us; 5us; 19us; 1us; 65535us; 20us; 21us; 2us; 65535us; 4us; 20us; 5us; 20us; 1us; 65535us; 21us; 22us; 1us; 65535us; 5us; 6us; 4us; 65535us; 31us; 32us; 33us; 34us; 36us; 37us; 38us; 39us; 4us; 65535us; 31us; 35us; 33us; 35us; 36us; 35us; 38us; 35us; 1us; 65535us; 6us; 7us; 1us; 65535us; 49us; 50us; 6us; 65535us; 31us; 44us; 33us; 44us; 36us; 44us; 38us; 44us; 41us; 42us; 45us; 46us; 2us; 65535us; 40us; 41us; 44us; 45us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 7us; 16us; 18us; 21us; 23us; 26us; 28us; 30us; 35us; 40us; 42us; 44us; 51us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 2us; 1us; 3us; 1us; 1us; 2us; 1us; 10us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 4us; 3us; 5us; 6us; 7us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 15us; 1us; 15us; 1us; 17us; 1us; 17us; 1us; 19us; 1us; 19us; 3us; 20us; 21us; 22us; 1us; 21us; 1us; 21us; 1us; 22us; 1us; 22us; 2us; 23us; 24us; 2us; 23us; 24us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 25us; 1us; 25us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 29us; 1us; 30us; 1us; 31us; 1us; 32us; 1us; 33us; 1us; 34us; 1us; 35us; 1us; 36us; 1us; 37us; 1us; 38us; 1us; 39us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 9us; 11us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 78us; 80us; 82us; 84us; 86us; 89us; 92us; 94us; 96us; 98us; 100us; 102us; 104us; 106us; 108us; 110us; 112us; 114us; 116us; 118us; 120us; 122us; 124us; 126us; 128us; 130us; |]
let _fsyacc_action_rows = 62
let _fsyacc_actionTableElements = [|1us; 32768us; 18us; 2us; 0us; 49152us; 1us; 32768us; 24us; 14us; 2us; 32768us; 2us; 11us; 19us; 9us; 3us; 16392us; 15us; 25us; 16us; 29us; 17us; 27us; 4us; 16402us; 15us; 25us; 16us; 29us; 17us; 27us; 21us; 33us; 1us; 16410us; 22us; 47us; 1us; 32768us; 0us; 8us; 0us; 16385us; 1us; 32768us; 24us; 10us; 0us; 16386us; 1us; 32768us; 24us; 14us; 0us; 16387us; 0us; 16388us; 2us; 16391us; 1us; 15us; 24us; 17us; 1us; 32768us; 24us; 16us; 0us; 16389us; 0us; 16390us; 0us; 16393us; 0us; 16394us; 1us; 32768us; 24us; 23us; 1us; 16400us; 13us; 31us; 0us; 16395us; 1us; 32768us; 24us; 24us; 0us; 16396us; 1us; 32768us; 14us; 26us; 0us; 16397us; 1us; 32768us; 14us; 28us; 0us; 16398us; 1us; 32768us; 14us; 30us; 0us; 16399us; 4us; 32768us; 24us; 14us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16401us; 4us; 32768us; 24us; 14us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16403us; 2us; 16404us; 11us; 36us; 12us; 38us; 4us; 32768us; 24us; 14us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16405us; 4us; 32768us; 24us; 14us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16406us; 6us; 32768us; 3us; 56us; 4us; 58us; 5us; 60us; 6us; 59us; 7us; 61us; 8us; 57us; 4us; 32768us; 24us; 14us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16407us; 0us; 16408us; 6us; 32768us; 3us; 56us; 4us; 58us; 5us; 60us; 6us; 59us; 7us; 61us; 8us; 57us; 3us; 32768us; 25us; 55us; 26us; 54us; 27us; 53us; 0us; 16409us; 1us; 32768us; 23us; 48us; 1us; 32768us; 24us; 14us; 2us; 16412us; 9us; 51us; 10us; 52us; 0us; 16411us; 0us; 16413us; 0us; 16414us; 0us; 16415us; 0us; 16416us; 0us; 16417us; 0us; 16418us; 0us; 16419us; 0us; 16420us; 0us; 16421us; 0us; 16422us; 0us; 16423us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 8us; 12us; 17us; 19us; 21us; 22us; 24us; 25us; 27us; 28us; 29us; 32us; 34us; 35us; 36us; 37us; 38us; 40us; 42us; 43us; 45us; 46us; 48us; 49us; 51us; 52us; 54us; 55us; 60us; 61us; 66us; 67us; 70us; 75us; 76us; 81us; 82us; 89us; 94us; 95us; 96us; 103us; 107us; 108us; 110us; 112us; 115us; 116us; 117us; 118us; 119us; 120us; 121us; 122us; 123us; 124us; 125us; 126us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 7us; 2us; 3us; 1us; 3us; 2us; 1us; 0us; 1us; 2us; 3us; 2us; 2us; 2us; 2us; 0us; 2us; 0us; 2us; 1us; 3us; 3us; 3us; 3us; 3us; 0us; 4us; 0us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 4us; 4us; 4us; 5us; 5us; 5us; 6us; 7us; 8us; 8us; 8us; 9us; 9us; 10us; 10us; 11us; 11us; 11us; 12us; 12us; 12us; 13us; 13us; 14us; 14us; 14us; 15us; 15us; 15us; 16us; 16us; 16us; 16us; 16us; 16us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16385us; 65535us; 16386us; 65535us; 16387us; 16388us; 65535us; 65535us; 16389us; 16390us; 16393us; 16394us; 65535us; 65535us; 16395us; 65535us; 16396us; 65535us; 16397us; 65535us; 16398us; 65535us; 16399us; 65535us; 16401us; 65535us; 16403us; 65535us; 65535us; 16405us; 65535us; 16406us; 65535us; 65535us; 16407us; 16408us; 65535us; 65535us; 16409us; 65535us; 65535us; 65535us; 16411us; 16413us; 16414us; 16415us; 16416us; 16417us; 16418us; 16419us; 16420us; 16421us; 16422us; 16423us; |]
let _fsyacc_reductions ()  =    [| 
# 283 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : SqlQuery.SqlQuery)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 292 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'columns)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'fromRule)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinRuleList)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereRule)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'orderRule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                       
                                             {Table = _3;
                                             Columns = _2;
                                             Joins = _4;
                                             Where = _5;
                                             Order = _6;}
                                         
                   )
# 30 "Parser.fsy"
                 : SqlQuery.SqlQuery));
# 313 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                       _2 
                   )
# 39 "Parser.fsy"
                 : 'fromRule));
# 324 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'columns)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                  _3::_1 
                   )
# 42 "Parser.fsy"
                 : 'columns));
# 336 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                   [_1] 
                   )
# 43 "Parser.fsy"
                 : 'columns));
# 347 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                       { TableRef = _1; ColumnName = _3; } 
                   )
# 45 "Parser.fsy"
                 : 'column));
# 359 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                    { TableRef = _1; ColumnName = _2; } 
                   )
# 46 "Parser.fsy"
                 : 'column));
# 371 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                 { TableRef = _1; ColumnName = _1; } 
                   )
# 47 "Parser.fsy"
                 : 'column));
# 382 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                               [] 
                   )
# 50 "Parser.fsy"
                 : 'joinRuleList));
# 392 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinRule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                         [_1] 
                   )
# 51 "Parser.fsy"
                 : 'joinRuleList));
# 403 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinRuleList)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinRule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                   _2::_1 
                   )
# 52 "Parser.fsy"
                 : 'joinRuleList));
# 415 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinType)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinTable)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'joinWhereExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "Parser.fsy"
                                                             _2, _1, _3 
                   )
# 55 "Parser.fsy"
                 : 'joinRule));
# 428 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                      {Ref = _2; Name= _2; } 
                   )
# 58 "Parser.fsy"
                 : 'joinTable));
# 440 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                           JoinType.INNER 
                   )
# 61 "Parser.fsy"
                 : 'joinType));
# 450 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                        JoinType.RIGHT 
                   )
# 62 "Parser.fsy"
                 : 'joinType));
# 460 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                                        JoinType.LEFT 
                   )
# 63 "Parser.fsy"
                 : 'joinType));
# 470 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Parser.fsy"
                                            None 
                   )
# 66 "Parser.fsy"
                 : 'joinWhereExpr));
# 480 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Parser.fsy"
                                         Some(_2) 
                   )
# 67 "Parser.fsy"
                 : 'joinWhereExpr));
# 491 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                               None 
                   )
# 70 "Parser.fsy"
                 : 'whereRule));
# 501 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                            Some(_2) 
                   )
# 71 "Parser.fsy"
                 : 'whereRule));
# 512 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereOp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Parser.fsy"
                                                     _1 
                   )
# 74 "Parser.fsy"
                 : 'whereExpr));
# 523 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereOp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                                                  And(_1, _3) 
                   )
# 75 "Parser.fsy"
                 : 'whereExpr));
# 535 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereOp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'whereExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                                  Or(_1, _3) 
                   )
# 76 "Parser.fsy"
                 : 'whereExpr));
# 547 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'op)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Parser.fsy"
                                               Expr(RefValue(_2, _1, _3)) 
                   )
# 79 "Parser.fsy"
                 : 'whereOp));
# 560 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'op)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                               Expr(RefOnly(_2, _1, _3)) 
                   )
# 80 "Parser.fsy"
                 : 'whereOp));
# 573 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'value)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'op)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Parser.fsy"
                                               Expr(Value(_2, _1, _3)) 
                   )
# 81 "Parser.fsy"
                 : 'whereOp));
# 586 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "Parser.fsy"
                                                                  None 
                   )
# 84 "Parser.fsy"
                 : 'orderRule));
# 596 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'column)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'orderDirectionRule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "Parser.fsy"
                                                               Some(_4, _3) 
                   )
# 85 "Parser.fsy"
                 : 'orderRule));
# 608 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "Parser.fsy"
                                    OrderDirection.ASC 
                   )
# 88 "Parser.fsy"
                 : 'orderDirectionRule));
# 618 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 89 "Parser.fsy"
                                 OrderDirection.ASC 
                   )
# 89 "Parser.fsy"
                 : 'orderDirectionRule));
# 628 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Parser.fsy"
                                 OrderDirection.DESC 
                   )
# 90 "Parser.fsy"
                 : 'orderDirectionRule));
# 638 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "Parser.fsy"
                                      Int(_1) 
                   )
# 93 "Parser.fsy"
                 : 'value));
# 649 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "Parser.fsy"
                                      String(_1) 
                   )
# 94 "Parser.fsy"
                 : 'value));
# 660 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "Parser.fsy"
                                      Bool(_1) 
                   )
# 95 "Parser.fsy"
                 : 'value));
# 671 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "Parser.fsy"
                                  Eq 
                   )
# 98 "Parser.fsy"
                 : 'op));
# 681 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "Parser.fsy"
                               Neq 
                   )
# 99 "Parser.fsy"
                 : 'op));
# 691 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 100 "Parser.fsy"
                               Lt 
                   )
# 100 "Parser.fsy"
                 : 'op));
# 701 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 101 "Parser.fsy"
                               Gt 
                   )
# 101 "Parser.fsy"
                 : 'op));
# 711 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 102 "Parser.fsy"
                               Leq 
                   )
# 102 "Parser.fsy"
                 : 'op));
# 721 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 103 "Parser.fsy"
                               Geq 
                   )
# 103 "Parser.fsy"
                 : 'op));
|]
# 732 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 31;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : SqlQuery.SqlQuery =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
