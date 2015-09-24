// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Akka.Actor
open Akka.FSharp
open System;
open Messages
open Thing
open OutputHandler
open ConnectionHandler
open System.Net

let system = System.create "my-system" (Configuration.load())

let server = spawn system "server" (mudService (new IPEndPoint(IPAddress.Any, 8090)))

let output = spawn system "output" (outputHandler)
let kitchen = spawn system "kitchen" (thing "the kitchen")
let livingroom = spawn system "livingroom" (thing "the living room")
let gandalf = spawn system "gandalf" (thing "Gandalf")
let goblin = spawn system "goblin" (thing "a goblin")
let player = spawn system "player" (thing "Player1")
let sword = spawn system "sword" (thing "a sword")
let helmet = spawn system "helmet" (thing "a rusty helmet")
let backpack = spawn system "backpack" (thing "a brown leather backpack")

kitchen <! AddExit({name="north";ref=livingroom})
livingroom <! AddExit({name="south";ref=kitchen})

player <! SetOutput(output)
player <! SetContainerByActorRef(kitchen)
goblin <! SetContainerByActorRef(kitchen)
gandalf <! SetContainerByActorRef(kitchen)
sword <! SetContainerByActorRef(kitchen)
helmet <! SetContainerByActorRef(kitchen)
backpack <! SetContainerByActorRef(livingroom)


while true do
    let input = Console.ReadLine()
    let parts = input.Split([|' '|],2,StringSplitOptions.RemoveEmptyEntries)
    let command = parts |> Seq.tryHead
    let message = parts |> Seq.skip(1) |> Seq.tryLast
    match command, message with
    | Some(str),Some(msg) ->
        match str with
        | "say"
        | "s" -> player <! Say(msg)
        | "take"
        | "get"
        | "t" -> player <! Take(msg)
        | "enter" -> player <! Enter(msg)
        | "drop"
        | "d" -> player <! Drop(msg)
        | "put" when msg.Contains(" in ") -> 
            let parts = msg.Split([|" in "|],2,StringSplitOptions.RemoveEmptyEntries)
            let target = parts.[0]
            let container = parts.[1]
            player <! Put(target,container)
        | other -> printfn "unknown command %A" other
    | Some(str),None ->        
        match str with
        | "look"
        | "l" -> player <! Look 
        | "exit" -> player <! Exit
        | "inventory"
        | "inv" -> player <! Inventory  
        | "where" -> player <! Where
        | "north"
        | "n" -> player <! Go("north")
        | "south"
        | "s" -> player <! Go("south")
        | "east"
        | "e" -> player <! Go("east")
        | "west"
        | "w" -> player <! Go("west")
        | other -> printfn "unknown command %A" other
    | _ -> ignore()