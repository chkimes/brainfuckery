module LowLevel
open HighLevel
open System.Collections.Generic

// All operations implicitly act against the current pointer location
type LowLevelBf = LMoveToPointer of int
                | LAddInteger of byte
                | LInc
                | LDec
                | LWhileBegin
                | LWhileEnd
                | LCall of string

type LowLevelMethodDef = { Name: string; Variables: IDictionary<string, int>; Parameters: string list; Code: LowLevelBf list }

let getLoc l (vars: IDictionary<string, int>) =
    match l with
    | LocalVariable(var) -> vars.Item(var)
    | MethodParam(i) -> i + 1
    

let highToLow vars high =
    match high with
    | HSetToZero(l) -> seq { yield LMoveToPointer (getLoc l vars) ; yield LWhileBegin ; yield LDec; yield LWhileEnd }
    | HAddInteger(l, add) -> seq { yield LMoveToPointer (getLoc l vars) ; yield LAddInteger add }
    | HCall(l, m) -> seq { yield LMoveToPointer (getLoc l vars) ; yield LCall m }
    | HCopy(src, dst) ->
        seq {
            // set temp to 0
            yield LMoveToPointer 0
            yield LWhileBegin
            yield LDec
            yield LWhileEnd

            // set dst to 0
            yield LMoveToPointer (getLoc dst vars)
            yield LWhileBegin
            yield LDec
            yield LWhileEnd

            // copy from src to temp and dst
            yield LMoveToPointer (getLoc src vars)
            yield LWhileBegin
            yield LDec
            yield LMoveToPointer 0
            yield LInc
            yield LMoveToPointer (getLoc dst vars)
            yield LInc
            yield LMoveToPointer (getLoc src vars)
            yield LWhileEnd

            // copy from tmp to src
            yield LMoveToPointer 0
            yield LWhileBegin
            yield LDec
            yield LMoveToPointer (getLoc src vars)
            yield LInc
            yield LMoveToPointer 0
            yield LWhileEnd
        }
    | HWhile(l) -> failwith "unsupported"

let highMethodToLowMethod (highMethod : HighLevelMethodDef) =
    let endAtEntryPoint = (fun ls -> Seq.append ls [LMoveToPointer -highMethod.Variables.Count])
    let transformed = highMethod.Code |> Seq.map (highToLow highMethod.Variables) |> Seq.concat |> endAtEntryPoint |> List.ofSeq
    { Name = highMethod.Name; Variables = highMethod.Variables; Parameters = highMethod.Parameters; Code = transformed }