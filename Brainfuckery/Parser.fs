module Parser
open FParsec

type Expr =
          | MethodCall of string * Expr list
          | Variable of string
          | Integer of byte
type Statement =
              | Assignment of string * Expr * bool
              | WhileLoop of string * Block
              | MethodCallStatement of string * Expr list
              | Return of Expr
            //| If of Expr * Block * Block option

and Block = Statement list
type MethodDefinition = string * string list * Block
type Program = MethodDefinition list

let identifier = many1Satisfy isLetter

let expr, exprRef = createParserForwardedToRef<Expr, unit>()
let methodCallTuple = spaces >>. identifier .>> spaces .>> pchar '(' .>> spaces .>>. sepBy expr (pchar ',' >>. spaces) .>> pchar ')'
let methodCallExpr = methodCallTuple |>> MethodCall
let variable = spaces >>. identifier |>> Variable
let integer = spaces >>. puint8 |>> Integer

do exprRef := choice [| attempt methodCallExpr ; attempt variable ; attempt integer |]

let block, blockRef = createParserForwardedToRef<Block, unit>()
let assignment = spaces >>. opt (pstring "let") .>> spaces .>>. identifier .>> spaces .>> pchar '=' .>> spaces .>>. expr .>> spaces .>> pchar ';' |>> (fun ((l, i), exprs) -> Assignment (i, exprs, Option.isSome l))
let whileLoop = spaces >>. pstring "while" >>. spaces >>. pchar '(' >>. spaces >>. identifier .>> spaces .>> pchar ')' .>>. block |>> WhileLoop
let methodCallStatement = methodCallTuple .>> spaces .>> pchar ';' |>> MethodCallStatement
let returnStatement = spaces >>. pstring "return" >>. spaces >>. expr .>> spaces .>> pchar ';' |>> Return

let statement = choice [| attempt assignment ; attempt whileLoop ; attempt methodCallStatement ; attempt returnStatement |]
do blockRef := spaces >>. pchar '{' >>. many statement .>> spaces .>> pchar '}'

let methodDefinition = spaces >>. pstring "def " >>. spaces >>. identifier .>> spaces .>> pchar '(' .>>. sepBy identifier (pchar ',' >>. spaces) .>> pchar ')' .>>. block |>> (fun ((x, y), z) -> x, y, z )
let program = many methodDefinition

let parse str =
    match run program str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, error, _) -> failwith errorMsg
