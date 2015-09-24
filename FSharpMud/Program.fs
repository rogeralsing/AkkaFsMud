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

let output = spawn system "output" (outputHandler)
let kitchen = spawn system "kitchen" (thing "the kitchen")
let livingroom = spawn system "livingroom" (thing "the living room")
let gandalf = spawn system "gandalf" (thing "Gandalf")
let goblin = spawn system "goblin" (thing "a goblin")
let player = spawn system "player" (thing "Player1")
let sword = spawn system "sword" (thing "a sword")
let helmet = spawn system "helmet" (thing "a rusty helmet")
let backpack = spawn system "backpack" (thing "a brown leather backpack")

let server = spawn system "server" (mudService kitchen (new IPEndPoint(IPAddress.Any, 8090)))

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
    handleInput player input