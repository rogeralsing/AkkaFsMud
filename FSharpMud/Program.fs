open Actors
open Akka.FSharp
open Messages
open System.Net
open System;

let system = System.create "my-system" (Configuration.load())

let output = spawn system "output" (ConsoleOutput.consoleOutputHandler)
let world = spawn system "wilderness" (container "the wilderness" false false)
let kitchen = spawn system "kitchen" (container "the kitchen" true false)
let livingroom = spawn system "livingroom" (container "the living room" true false)
let gandalf = spawn system "gandalf" (living "Gandalf")
let goblin = spawn system "goblin" (npc "a goblin")
let ogre = spawn system "ogre" (npc "an ogre")
let player = spawn system "player" (player "Admin")
let sword = spawn system "sword" (thing "a sword")
let helmet = spawn system "helmet" (thing "a rusty helmet")
let backpack = spawn system "backpack" (container "a brown leather backpack" true true)

let server = spawn system "server" (ConnectionHandler.mudService kitchen (new IPEndPoint(IPAddress.Any, 8090)))

kitchen <! AddExit({name="north";ref=livingroom})
livingroom <! AddExit({name="south";ref=kitchen})

kitchen <! SetContainerByActorRef(world)
livingroom <! SetContainerByActorRef(world)
player <! SetOutput(output)
player <! SetContainerByActorRef(kitchen)
goblin <! SetContainerByActorRef(kitchen)
ogre <! SetContainerByActorRef(livingroom)
gandalf <! SetContainerByActorRef(kitchen)
sword <! SetContainerByActorRef(kitchen)
helmet <! SetContainerByActorRef(kitchen)
backpack <! SetContainerByActorRef(livingroom)

while true do
    let input = Console.ReadLine()
    InputHandler.handleInput player input