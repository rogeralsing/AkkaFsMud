module Utils

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let RemovePrefix (name:string) =
    match name.ToLowerInvariant() with
    | Prefix "an " rest -> rest
    | Prefix "a " rest -> rest
    | Prefix "the " rest -> rest
    | rest -> rest

let workflow asy = 
    Async.StartImmediate <| asy