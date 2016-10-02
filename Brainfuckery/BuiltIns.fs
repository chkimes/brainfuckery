module BuiltIns
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
