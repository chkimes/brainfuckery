module CompileBf
open LowLevel
open System.Collections.Generic

type CompiledMethodDef = { Name: string; Variables: IDictionary<string, int>; Parameters: string list; Code: string }
type CompilationState = { CurrentPointer: int; Statement: string }

type MethodRef = Compiled of CompiledMethodDef
               | LowLevel of LowLevelMethodDef

let refName methodRef =
    match methodRef with
    | Compiled(c) -> c.Name
    | LowLevel(l) -> l.Name

let moveTo current dst =
    if current < dst then String.replicate (dst - current) ">"
    elif current > dst then String.replicate (current - dst) "<"
    else ""

let rec lowToBf (defs : IDictionary<string, MethodRef>) state low =
    let code = match low with
               | LMoveToPointer(i) -> moveTo state.CurrentPointer i
               | LAddInteger(i) -> String.replicate (int i) "+"
               | LInc -> "+"
               | LDec -> "-"
               | LWhileBegin -> "["
               | LWhileEnd -> "]"
               | LCall(m) ->
                    let meth  = defs.Item m
                    match meth with
                    | Compiled(c) -> c.Code
                    | LowLevel(l) -> (lowMethodToBfMethod defs l).Code // super-super naive inlining option
    let nextPointer = match low with LMoveToPointer(i) -> i | _ -> state.CurrentPointer
    { CurrentPointer = nextPointer ; Statement = code }

and lowMethodToBfMethod (defs : IDictionary<string, MethodRef>) (lowMethod : LowLevelMethodDef) =
    let state = { CurrentPointer = -lowMethod.Variables.Count ; Statement = "" }
    let transformed = lowMethod.Code |> Seq.scan (lowToBf defs) state |> Seq.map (fun s -> s.Statement) |> Seq.reduce (+)
    { Name = lowMethod.Name; Variables = lowMethod.Variables; Parameters = lowMethod.Parameters; Code = transformed}