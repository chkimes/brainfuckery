module HighLevel
open Parser
open System.Collections.Generic

type PointerLoc = LocalVariable of string
                | MethodParam of int

type HighLevelBf = HSetToZero of PointerLoc
                 | HAddInteger of PointerLoc * byte
                 | HCopy of PointerLoc * PointerLoc
                 | HCall of PointerLoc * string
                 | HWhile of PointerLoc

type HighLevelMethodDef = { Name: string; Variables: IDictionary<string, int>; Parameters: string list; Code: HighLevelBf list }



let setIntValue loc value = seq { yield HSetToZero loc ; yield HAddInteger (loc, value) }

let rec highLevelExpr startingParam expr =
    match expr with
    | Integer(a) -> setIntValue (MethodParam startingParam) a
    | Variable(v) -> seq { yield HCopy (LocalVariable v, MethodParam startingParam) }
    | MethodCall(m, exprs) ->
        let childExprs = exprs |> Seq.mapi (fun p e -> highLevelExpr (p + startingParam) e) |> Seq.concat 
        seq { yield! childExprs ; yield HCall (MethodParam startingParam, m) }

let highLevelAssignment assignTo expr =
    match expr with
        | Integer(a) -> setIntValue assignTo a
        | Variable(v) -> seq { yield HCopy ((LocalVariable v), assignTo) }
        | MethodCall(_) as m -> seq { yield! highLevelExpr 0 m ; yield HCopy (MethodParam 0, assignTo) }

let generateHighLevel varCount statement =
    match statement with
        | Assignment(a, expr, n) -> highLevelAssignment (LocalVariable a) expr
        | MethodCallStatement(m, exprs) -> highLevelExpr 0 (MethodCall (m, exprs))
        | Return(expr) -> highLevelAssignment (MethodParam -(1 + varCount)) expr
        | WhileLoop(_) -> failwith "Unsupported statement type"

let getVars parameters statements =
    let assignmentNames =
        statements
        |> Seq.choose (fun x -> match x with Assignment(name, expr, true) -> Some name | _ -> None)
    let vars = Seq.append parameters assignmentNames
    let grouped = vars |> Seq.groupBy id
    let duplicates = grouped |> Seq.filter (fun (x, xs) -> Seq.length xs > 1)
    if not (Seq.isEmpty duplicates) then failwith ("Duplicate variable declaration: " + fst (Seq.head duplicates))

    let count = vars |> Seq.length
    vars |> Seq.mapi (fun i v -> v, (i - count)) |> dict

let astMethodToHighMethod (methodDef : MethodDefinition) =
    let name, parameters, statements = methodDef
    let vars = getVars parameters statements
    let transformed = statements |> Seq.map (generateHighLevel vars.Count) |> Seq.concat |> List.ofSeq
    { Name = name; Variables = vars; Parameters = parameters; Code = transformed }
