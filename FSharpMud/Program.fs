open Akka.FSharp
open System;
open Messages
open Thing
open OutputHandler
open ConnectionHandler
open System.Net
open InputHandler

let system = System.create "my-system" (Configuration.load())

let output = spawn system "output" (outputHandler)
let world = spawn system "wilderness" (container "the wilderness")
let kitchen = spawn system "kitchen" (container "the kitchen")
let livingroom = spawn system "livingroom" (container "the living room")
let gandalf = spawn system "gandalf" (living "Gandalf")
let goblin = spawn system "goblin" (living "a goblin")
let player = spawn system "player" (living "Admin")
let sword = spawn system "sword" (living "a sword")
let helmet = spawn system "helmet" (living "a rusty helmet")
let backpack = spawn system "backpack" (container "a brown leather backpack")

let server = spawn system "server" (mudService kitchen (new IPEndPoint(IPAddress.Any, 8090)))

kitchen <! AddExit({name="north";ref=livingroom})
livingroom <! AddExit({name="south";ref=kitchen})

kitchen <! SetContainerByActorRef(world)
livingroom <! SetContainerByActorRef(world)
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