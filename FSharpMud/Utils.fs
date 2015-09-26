module Utils
open System

let (|Prefix|_|) (p : string) (s : string) = 
    if s.StartsWith(p) then Some(s.Substring(p.Length))
    else None

let removePrefix(name : string) = 
    match name.ToLowerInvariant() with
    | Prefix "an " rest -> rest
    | Prefix "a " rest -> rest
    | Prefix "the " rest -> rest
    | rest -> rest

let joinStrings (strs : seq<string>) = 
    let strings = strs |> Seq.toArray
    match strings.Length with
    | 0 -> "nothing"
    | 1 -> strings.[0]
    | _ -> 
        let allButLast = strings |> Seq.take (strings.Length - 1)
        let first = String.Join(", ", allButLast)
        let last = Array.last (strings)
        first + " and " + last

