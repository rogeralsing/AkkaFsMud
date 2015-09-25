module InputHandler
open Messages
open System
open Akka.FSharp

let handleInput player (input : string) = 
    let parts = input.Split([| ' ' |], 2, StringSplitOptions.None)
    let command = parts |> Seq.tryHead
    
    let message = 
        parts
        |> Seq.skip (1)
        |> Seq.tryLast
    match command, message with
    | Some(str), Some(msg) -> 
        match str with
        | "say" | "s" -> player <! Say(msg)
        | "yell" | "y" -> player <! Yell(msg)
        | "take" | "get" | "t" -> player <! Take(msg)
        | "enter" -> player <! Enter(msg)
        | "drop" | "d" -> player <! Drop(msg)
        | "put" when msg.Contains(" in ") -> 
            let parts = msg.Split([| " in " |], 2, StringSplitOptions.RemoveEmptyEntries)
            let target = parts.[0]
            let container = parts.[1]
            player <! Put(target, container)
        | other -> printfn "unknown command %A" other
    | Some(str), None -> 
        match str with
        | "look" | "l" -> player <! Look
        | "exit" -> player <! Exit
        | "inventory" | "inv" -> player <! Inventory
        | "north" | "n" -> player <! Go("north")
        | "south" | "s" -> player <! Go("south")
        | "east" | "e" -> player <! Go("east")
        | "west" | "w" -> player <! Go("west")
        | other -> printfn "unknown command %A" other
    | _ -> ignore()