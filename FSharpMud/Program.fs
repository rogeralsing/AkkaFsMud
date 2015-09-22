// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Akka.Actor
open Akka.FSharp
open System;
open Messages
open Thing
open OutputHandler

let system = System.create "my-system" (Configuration.load())

let output = spawn system "output" (outputHandler)
let kitchen = spawn system "kitchen" (thing "the kitchen")
let gandalf = spawn system "gandalf" (thing "Gandalf")
let goblin = spawn system "goblin" (thing "a goblin")
let player = spawn system "player" (thing "Player1")
let sword = spawn system "sword" (thing "a sword")
let helmet = spawn system "helmet" (thing "a rusty helmet")

player <! SetOutput(output)
player <! SetContainer(kitchen)
goblin <! SetContainer(kitchen)
gandalf <! SetContainer(kitchen)
sword <! SetContainer(kitchen)
helmet <! SetContainer(kitchen)

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
        | other -> printfn "unknown command %A" other
    | Some(str),None ->        
        match str with
        | "look"
        | "l" -> player <! Look   
        | other -> printfn "unknown command %A" other
    | _ -> ignore()