// Implementation file for parser generated by fsyacc
module TexParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "TexParser.fsy"

open TexAst
open Microsoft.FSharp.Text.Lexing

# 11 "TexParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | PI
  | EULERNUM
  | FRAC
  | SQRT
  | SUM
  | PROD
  | INTEGRAL
  | TIMES
  | INFTY
  | TO
  | LIM
  | BMOD
  | PLUS
  | SUB
  | MUL
  | DIV
  | LSQBRACE
  | RSQBRACE
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | EXCL
  | CARET
  | EQ
  | UNDERSCORE
  | SLASH
  | COMMA
  | VAR of (string)
  | DIFF of (string)
  | INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_PI
    | TOKEN_EULERNUM
    | TOKEN_FRAC
    | TOKEN_SQRT
    | TOKEN_SUM
    | TOKEN_PROD
    | TOKEN_INTEGRAL
    | TOKEN_TIMES
    | TOKEN_INFTY
    | TOKEN_TO
    | TOKEN_LIM
    | TOKEN_BMOD
    | TOKEN_PLUS
    | TOKEN_SUB
    | TOKEN_MUL
    | TOKEN_DIV
    | TOKEN_LSQBRACE
    | TOKEN_RSQBRACE
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LCURLY
    | TOKEN_RCURLY
    | TOKEN_EXCL
    | TOKEN_CARET
    | TOKEN_EQ
    | TOKEN_UNDERSCORE
    | TOKEN_SLASH
    | TOKEN_COMMA
    | TOKEN_VAR
    | TOKEN_DIFF
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_constant
    | NONTERM_sum
    | NONTERM_prod
    | NONTERM_fact
    | NONTERM_integral
    | NONTERM_lowPriorityBinaryOperator
    | NONTERM_mediumPriorityBinaryOperator
    | NONTERM_highPriorityFunction
    | NONTERM_mediumPriorityBinaryExpression
    | NONTERM_lowPriorityBinaryExpression
    | NONTERM_argumentSet

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | PI  -> 1 
  | EULERNUM  -> 2 
  | FRAC  -> 3 
  | SQRT  -> 4 
  | SUM  -> 5 
  | PROD  -> 6 
  | INTEGRAL  -> 7 
  | TIMES  -> 8 
  | INFTY  -> 9 
  | TO  -> 10 
  | LIM  -> 11 
  | BMOD  -> 12 
  | PLUS  -> 13 
  | SUB  -> 14 
  | MUL  -> 15 
  | DIV  -> 16 
  | LSQBRACE  -> 17 
  | RSQBRACE  -> 18 
  | LPAREN  -> 19 
  | RPAREN  -> 20 
  | LCURLY  -> 21 
  | RCURLY  -> 22 
  | EXCL  -> 23 
  | CARET  -> 24 
  | EQ  -> 25 
  | UNDERSCORE  -> 26 
  | SLASH  -> 27 
  | COMMA  -> 28 
  | VAR _ -> 29 
  | DIFF _ -> 30 
  | INT _ -> 31 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_PI 
  | 2 -> TOKEN_EULERNUM 
  | 3 -> TOKEN_FRAC 
  | 4 -> TOKEN_SQRT 
  | 5 -> TOKEN_SUM 
  | 6 -> TOKEN_PROD 
  | 7 -> TOKEN_INTEGRAL 
  | 8 -> TOKEN_TIMES 
  | 9 -> TOKEN_INFTY 
  | 10 -> TOKEN_TO 
  | 11 -> TOKEN_LIM 
  | 12 -> TOKEN_BMOD 
  | 13 -> TOKEN_PLUS 
  | 14 -> TOKEN_SUB 
  | 15 -> TOKEN_MUL 
  | 16 -> TOKEN_DIV 
  | 17 -> TOKEN_LSQBRACE 
  | 18 -> TOKEN_RSQBRACE 
  | 19 -> TOKEN_LPAREN 
  | 20 -> TOKEN_RPAREN 
  | 21 -> TOKEN_LCURLY 
  | 22 -> TOKEN_RCURLY 
  | 23 -> TOKEN_EXCL 
  | 24 -> TOKEN_CARET 
  | 25 -> TOKEN_EQ 
  | 26 -> TOKEN_UNDERSCORE 
  | 27 -> TOKEN_SLASH 
  | 28 -> TOKEN_COMMA 
  | 29 -> TOKEN_VAR 
  | 30 -> TOKEN_DIFF 
  | 31 -> TOKEN_INT 
  | 34 -> TOKEN_end_of_input
  | 32 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_expression 
    | 3 -> NONTERM_expression 
    | 4 -> NONTERM_expression 
    | 5 -> NONTERM_constant 
    | 6 -> NONTERM_constant 
    | 7 -> NONTERM_sum 
    | 8 -> NONTERM_prod 
    | 9 -> NONTERM_fact 
    | 10 -> NONTERM_integral 
    | 11 -> NONTERM_lowPriorityBinaryOperator 
    | 12 -> NONTERM_lowPriorityBinaryOperator 
    | 13 -> NONTERM_mediumPriorityBinaryOperator 
    | 14 -> NONTERM_mediumPriorityBinaryOperator 
    | 15 -> NONTERM_highPriorityFunction 
    | 16 -> NONTERM_highPriorityFunction 
    | 17 -> NONTERM_highPriorityFunction 
    | 18 -> NONTERM_highPriorityFunction 
    | 19 -> NONTERM_highPriorityFunction 
    | 20 -> NONTERM_highPriorityFunction 
    | 21 -> NONTERM_highPriorityFunction 
    | 22 -> NONTERM_highPriorityFunction 
    | 23 -> NONTERM_highPriorityFunction 
    | 24 -> NONTERM_highPriorityFunction 
    | 25 -> NONTERM_mediumPriorityBinaryExpression 
    | 26 -> NONTERM_mediumPriorityBinaryExpression 
    | 27 -> NONTERM_lowPriorityBinaryExpression 
    | 28 -> NONTERM_argumentSet 
    | 29 -> NONTERM_argumentSet 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 34 
let _fsyacc_tagOfErrorTerminal = 32

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | PI  -> "PI" 
  | EULERNUM  -> "EULERNUM" 
  | FRAC  -> "FRAC" 
  | SQRT  -> "SQRT" 
  | SUM  -> "SUM" 
  | PROD  -> "PROD" 
  | INTEGRAL  -> "INTEGRAL" 
  | TIMES  -> "TIMES" 
  | INFTY  -> "INFTY" 
  | TO  -> "TO" 
  | LIM  -> "LIM" 
  | BMOD  -> "BMOD" 
  | PLUS  -> "PLUS" 
  | SUB  -> "SUB" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | LSQBRACE  -> "LSQBRACE" 
  | RSQBRACE  -> "RSQBRACE" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | LCURLY  -> "LCURLY" 
  | RCURLY  -> "RCURLY" 
  | EXCL  -> "EXCL" 
  | CARET  -> "CARET" 
  | EQ  -> "EQ" 
  | UNDERSCORE  -> "UNDERSCORE" 
  | SLASH  -> "SLASH" 
  | COMMA  -> "COMMA" 
  | VAR _ -> "VAR" 
  | DIFF _ -> "DIFF" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | PI  -> (null : System.Object) 
  | EULERNUM  -> (null : System.Object) 
  | FRAC  -> (null : System.Object) 
  | SQRT  -> (null : System.Object) 
  | SUM  -> (null : System.Object) 
  | PROD  -> (null : System.Object) 
  | INTEGRAL  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | INFTY  -> (null : System.Object) 
  | TO  -> (null : System.Object) 
  | LIM  -> (null : System.Object) 
  | BMOD  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | SUB  -> (null : System.Object) 
  | MUL  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | LSQBRACE  -> (null : System.Object) 
  | RSQBRACE  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | LCURLY  -> (null : System.Object) 
  | RCURLY  -> (null : System.Object) 
  | EXCL  -> (null : System.Object) 
  | CARET  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | UNDERSCORE  -> (null : System.Object) 
  | SLASH  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DIFF _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 11us; 65535us; 0us; 2us; 11us; 59us; 15us; 59us; 23us; 24us; 27us; 28us; 30us; 31us; 39us; 40us; 49us; 50us; 52us; 53us; 61us; 58us; 62us; 60us; 13us; 65535us; 0us; 46us; 11us; 46us; 15us; 46us; 23us; 46us; 27us; 46us; 30us; 46us; 39us; 46us; 49us; 46us; 52us; 46us; 56us; 46us; 57us; 46us; 61us; 46us; 62us; 46us; 13us; 65535us; 0us; 42us; 11us; 42us; 15us; 42us; 23us; 42us; 27us; 42us; 30us; 42us; 39us; 42us; 49us; 42us; 52us; 42us; 56us; 42us; 57us; 42us; 61us; 42us; 62us; 42us; 13us; 65535us; 0us; 43us; 11us; 43us; 15us; 43us; 23us; 43us; 27us; 43us; 30us; 43us; 39us; 43us; 49us; 43us; 52us; 43us; 56us; 43us; 57us; 43us; 61us; 43us; 62us; 43us; 13us; 65535us; 0us; 44us; 11us; 44us; 15us; 44us; 23us; 44us; 27us; 44us; 30us; 44us; 39us; 44us; 49us; 44us; 52us; 44us; 56us; 44us; 57us; 44us; 61us; 44us; 62us; 44us; 13us; 65535us; 0us; 45us; 11us; 45us; 15us; 45us; 23us; 45us; 27us; 45us; 30us; 45us; 39us; 45us; 49us; 45us; 52us; 45us; 56us; 45us; 57us; 45us; 61us; 45us; 62us; 45us; 10us; 65535us; 2us; 61us; 24us; 61us; 28us; 61us; 31us; 61us; 40us; 61us; 50us; 61us; 53us; 61us; 58us; 61us; 59us; 61us; 60us; 61us; 2us; 65535us; 5us; 57us; 6us; 56us; 13us; 65535us; 0us; 6us; 11us; 6us; 15us; 6us; 23us; 6us; 27us; 6us; 30us; 6us; 39us; 6us; 49us; 6us; 52us; 6us; 56us; 18us; 57us; 19us; 61us; 6us; 62us; 6us; 11us; 65535us; 0us; 5us; 11us; 5us; 15us; 5us; 23us; 5us; 27us; 5us; 30us; 5us; 39us; 5us; 49us; 5us; 52us; 5us; 61us; 5us; 62us; 5us; 11us; 65535us; 0us; 4us; 11us; 4us; 15us; 4us; 23us; 4us; 27us; 4us; 30us; 4us; 39us; 4us; 49us; 4us; 52us; 4us; 61us; 4us; 62us; 4us; 2us; 65535us; 11us; 12us; 15us; 16us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 15us; 29us; 43us; 57us; 71us; 85us; 96us; 99us; 113us; 125us; 137us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 27us; 1us; 1us; 1us; 2us; 2us; 3us; 26us; 3us; 4us; 9us; 25us; 1us; 5us; 1us; 6us; 4us; 7us; 8us; 10us; 23us; 1us; 7us; 1us; 7us; 2us; 7us; 29us; 1us; 7us; 1us; 8us; 1us; 8us; 2us; 8us; 29us; 1us; 8us; 2us; 9us; 25us; 2us; 9us; 26us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 2us; 10us; 27us; 1us; 10us; 1us; 10us; 1us; 10us; 2us; 10us; 27us; 1us; 10us; 1us; 10us; 2us; 10us; 27us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 2us; 16us; 27us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 23us; 2us; 23us; 27us; 1us; 23us; 1us; 23us; 2us; 23us; 27us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 2us; 27us; 27us; 2us; 27us; 28us; 2us; 27us; 29us; 1us; 27us; 1us; 29us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 11us; 14us; 18us; 20us; 22us; 27us; 29us; 31us; 34us; 36us; 38us; 40us; 43us; 45us; 48us; 51us; 53us; 55us; 57us; 59us; 62us; 64us; 66us; 68us; 71us; 73us; 75us; 78us; 80us; 82us; 84us; 86us; 88us; 90us; 92us; 94us; 97us; 99us; 101us; 103us; 105us; 107us; 109us; 111us; 113us; 115us; 118us; 120us; 122us; 125us; 127us; 129us; 131us; 133us; 136us; 139us; 142us; 144us; |]
let _fsyacc_action_rows = 63
let _fsyacc_actionTableElements = [|7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 0us; 49152us; 3us; 32768us; 0us; 3us; 13us; 34us; 14us; 35us; 0us; 16385us; 0us; 16386us; 2us; 16387us; 15us; 36us; 16us; 37us; 3us; 16388us; 15us; 36us; 16us; 37us; 23us; 20us; 0us; 16389us; 0us; 16390us; 4us; 32768us; 3us; 48us; 5us; 10us; 6us; 14us; 7us; 21us; 1us; 32768us; 21us; 11us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 2us; 32768us; 22us; 13us; 28us; 62us; 0us; 16391us; 1us; 32768us; 21us; 15us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 2us; 32768us; 22us; 17us; 28us; 62us; 0us; 16392us; 1us; 16409us; 23us; 20us; 1us; 16410us; 23us; 20us; 0us; 16393us; 1us; 32768us; 26us; 22us; 1us; 32768us; 21us; 23us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 22us; 25us; 1us; 32768us; 24us; 26us; 1us; 32768us; 21us; 27us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 22us; 29us; 1us; 32768us; 21us; 30us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 22us; 32us; 1us; 32768us; 30us; 33us; 0us; 16394us; 0us; 16395us; 0us; 16396us; 0us; 16397us; 0us; 16398us; 0us; 16399us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 20us; 41us; 0us; 16400us; 0us; 16401us; 0us; 16402us; 0us; 16403us; 0us; 16404us; 0us; 16405us; 0us; 16406us; 1us; 32768us; 21us; 49us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 22us; 51us; 1us; 32768us; 21us; 52us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 3us; 32768us; 13us; 34us; 14us; 35us; 22us; 54us; 0us; 16407us; 0us; 16408us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 2us; 16411us; 13us; 34us; 14us; 35us; 2us; 16412us; 13us; 34us; 14us; 35us; 2us; 16413us; 13us; 34us; 14us; 35us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; 7us; 32768us; 1us; 7us; 2us; 8us; 19us; 39us; 27us; 9us; 29us; 47us; 30us; 55us; 31us; 38us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 8us; 9us; 13us; 14us; 15us; 18us; 22us; 23us; 24us; 29us; 31us; 39us; 42us; 43us; 45us; 53us; 56us; 57us; 59us; 61us; 62us; 64us; 66us; 74us; 78us; 80us; 82us; 90us; 94us; 96us; 104us; 108us; 110us; 111us; 112us; 113us; 114us; 115us; 116us; 124us; 128us; 129us; 130us; 131us; 132us; 133us; 134us; 135us; 137us; 145us; 149us; 151us; 159us; 163us; 164us; 165us; 173us; 181us; 184us; 187us; 190us; 198us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 1us; 1us; 1us; 5us; 5us; 2us; 14us; 1us; 1us; 1us; 1us; 1us; 3us; 1us; 1us; 1us; 1us; 1us; 1us; 8us; 1us; 3us; 3us; 3us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; 3us; 4us; 5us; 6us; 7us; 8us; 8us; 9us; 9us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 11us; 11us; 12us; 13us; 13us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 65535us; 65535us; 16389us; 16390us; 65535us; 65535us; 65535us; 65535us; 16391us; 65535us; 65535us; 65535us; 16392us; 65535us; 65535us; 16393us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16394us; 16395us; 16396us; 16397us; 16398us; 16399us; 65535us; 65535us; 16400us; 16401us; 16402us; 16403us; 16404us; 16405us; 16406us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16407us; 16408us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 294 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : TexAst.Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 303 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 22 "TexParser.fsy"
                                       _1 
                   )
# 22 "TexParser.fsy"
                 : TexAst.Expr));
# 314 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'lowPriorityBinaryExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "TexParser.fsy"
                                                            _1 
                   )
# 25 "TexParser.fsy"
                 : 'expression));
# 325 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'mediumPriorityBinaryExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "TexParser.fsy"
                                                            _1 
                   )
# 26 "TexParser.fsy"
                 : 'expression));
# 336 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'highPriorityFunction)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "TexParser.fsy"
                                                         _1 
                   )
# 27 "TexParser.fsy"
                 : 'expression));
# 347 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "TexParser.fsy"
                                   Constant(Pi) 
                   )
# 30 "TexParser.fsy"
                 : 'constant));
# 357 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "TexParser.fsy"
                                   Constant(E) 
                   )
# 31 "TexParser.fsy"
                 : 'constant));
# 367 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'argumentSet)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "TexParser.fsy"
                                                                Sum(List.rev _4) 
                   )
# 34 "TexParser.fsy"
                 : 'sum));
# 378 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'argumentSet)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "TexParser.fsy"
                                                                Prod(List.rev _4) 
                   )
# 37 "TexParser.fsy"
                 : 'prod));
# 389 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'highPriorityFunction)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "TexParser.fsy"
                                                      Fact(_1) 
                   )
# 40 "TexParser.fsy"
                 : 'fact));
# 400 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _9 = (let data = parseState.GetInput(9) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _12 = (let data = parseState.GetInput(12) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _14 = (let data = parseState.GetInput(14) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "TexParser.fsy"
                                                                                                                                           Integral(_12, _14, _5, _9) 
                   )
# 43 "TexParser.fsy"
                 : 'integral));
# 414 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "TexParser.fsy"
                                   PLUS 
                   )
# 46 "TexParser.fsy"
                 : 'lowPriorityBinaryOperator));
# 424 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "TexParser.fsy"
                                   SUB 
                   )
# 47 "TexParser.fsy"
                 : 'lowPriorityBinaryOperator));
# 434 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "TexParser.fsy"
                                   MUL 
                   )
# 50 "TexParser.fsy"
                 : 'mediumPriorityBinaryOperator));
# 444 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "TexParser.fsy"
                                   DIV 
                   )
# 51 "TexParser.fsy"
                 : 'mediumPriorityBinaryOperator));
# 454 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "TexParser.fsy"
                                   Int(_1) 
                   )
# 54 "TexParser.fsy"
                 : 'highPriorityFunction));
# 465 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "TexParser.fsy"
                                                        _2 
                   )
# 55 "TexParser.fsy"
                 : 'highPriorityFunction));
# 476 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'sum)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "TexParser.fsy"
                                   _1 
                   )
# 56 "TexParser.fsy"
                 : 'highPriorityFunction));
# 487 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'prod)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "TexParser.fsy"
                                   _1 
                   )
# 57 "TexParser.fsy"
                 : 'highPriorityFunction));
# 498 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'fact)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "TexParser.fsy"
                                   _1 
                   )
# 58 "TexParser.fsy"
                 : 'highPriorityFunction));
# 509 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'integral)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "TexParser.fsy"
                                   _1 
                   )
# 59 "TexParser.fsy"
                 : 'highPriorityFunction));
# 520 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'constant)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "TexParser.fsy"
                                   _1 
                   )
# 60 "TexParser.fsy"
                 : 'highPriorityFunction));
# 531 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "TexParser.fsy"
                                   Var(_1) 
                   )
# 61 "TexParser.fsy"
                 : 'highPriorityFunction));
# 542 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "TexParser.fsy"
                                                                                            Div(_4, _7) 
                   )
# 62 "TexParser.fsy"
                 : 'highPriorityFunction));
# 554 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "TexParser.fsy"
                                   Diff(_1) 
                   )
# 63 "TexParser.fsy"
                 : 'highPriorityFunction));
# 565 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'highPriorityFunction)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'mediumPriorityBinaryOperator)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'highPriorityFunction)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "TexParser.fsy"
                                                                                                     
                                                                                                       match _2 with
                                                                                                           | MUL -> Mul(_1, _3)
                                                                                                           | DIV -> Div(_1, _3)
                                                                                                   
                   )
# 66 "TexParser.fsy"
                 : 'mediumPriorityBinaryExpression));
# 582 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'mediumPriorityBinaryExpression)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'mediumPriorityBinaryOperator)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'highPriorityFunction)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "TexParser.fsy"
                                                                                                               
                                                                                                                 match _2 with
                                                                                                                     | MUL -> Mul(_1, _3)
                                                                                                                     | DIV -> Div(_1, _3)
                                                                                                             
                   )
# 71 "TexParser.fsy"
                 : 'mediumPriorityBinaryExpression));
# 599 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'lowPriorityBinaryOperator)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "TexParser.fsy"
                                                                                 
                                                                                 match _2 with
                                                                                    | PLUS -> Plus(_1, _3)
                                                                                    | SUB -> Sub(_1, _3)
                                                                               
                   )
# 78 "TexParser.fsy"
                 : 'lowPriorityBinaryExpression));
# 616 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "TexParser.fsy"
                                         [_1] 
                   )
# 86 "TexParser.fsy"
                 : 'argumentSet));
# 627 "TexParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'argumentSet)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "TexParser.fsy"
                                                       _3::_1 
                   )
# 87 "TexParser.fsy"
                 : 'argumentSet));
|]
# 640 "TexParser.fs"
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
    numTerminals = 35;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : TexAst.Expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
