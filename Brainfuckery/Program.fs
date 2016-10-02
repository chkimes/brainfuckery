open Parser
open HighLevel
open LowLevel
open CompileBf
open System.Collections.Generic

let compiledAdd =
    { Name = "add"; Variables = new Dictionary<string, int>() ; Parameters = [ "a"; "b" ] ; Code = ">[-<+>]<" }

let compiledRead =
    { Name = "read"; Variables = new Dictionary<string, int>() ; Parameters = [] ; Code = "," }

let compiledWrite =
    { Name = "write"; Variables = new Dictionary<string, int>() ; Parameters = [ "a" ] ; Code = "." }

let builtIns =
    seq {
        yield compiledAdd
        yield compiledRead
        yield compiledWrite
    } |> Seq.map Compiled |> List.ofSeq

[<EntryPoint>]
let main argv = 
    let ast = parse "def main() { let a = 5; let b = 1; let c = a; let d = add(addOne(a), c); } def addOne(a) { return add(a, 1); }"
    let highLevel = ast 
    let lowLevel =
        ast
        |> Seq.map astMethodToHighMethod
        |> Seq.map highMethodToLowMethod
    let methodDict = lowLevel |> Seq.map LowLevel |> Seq.append builtIns |> Seq.map (fun l -> refName l, l) |> dict
    let main = match methodDict.Item("main") with
               | LowLevel(m) -> lowMethodToBfMethod methodDict m
               | Compiled(_) -> failwith "invalid"

    System.Console.WriteLine(main.Code) |> ignore
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
