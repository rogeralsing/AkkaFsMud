﻿module Thing
open Akka.Actor
open Akka.FSharp
open System;
open System.Collections.Generic
open System.Threading.Tasks
open Messages
open Utils
open Patterns

let thing (name:string) (mailbox: Actor<ThingMessage>)  = 
    let mutable container : IActorRef = ActorRefs.Nobody :> IActorRef
    let mutable output : IActorRef = ActorRefs.Nobody :> IActorRef
    let childFactory : IActorRefFactory = mailbox.Context :> IActorRefFactory

    let content = new HashSet<_>(HashIdentity.Reference)
    let self = mailbox.Self
    
    let notify message =
        self <! Notify(message)
        ignore()

    let containerNotify message except =
        self <! ContainerNotify(message,except)
        ignore()

    let rec loop() = actor {        
        let! message = mailbox.Receive()
        let sender = mailbox.Sender()
        match message with
        | SetOutput(o) -> output <- o
        | GetName -> sender <! name
        | Notify(message) -> output <! message      
        | SetContainer(cont) ->
            container <! ContainerRemove(self)
            container <- cont
            container <! ContainerAdd(self)

        | ContainerAdd(who) -> 
            if content.Add(who) then                 
                async {
                    let! name = who.Ask<string>(GetName,TimeSpan.FromSeconds(1.0))
                    containerNotify (Message ("{0} appears",[name])) [who; sender]
                } |> workflow

        | ContainerRemove(who) ->
            if content.Remove(who) then                
                async {
                    let! name = who.Ask<string>(GetName,TimeSpan.FromSeconds(1.0))
                    containerNotify (Message ("{0} disappears",[name])) [who; sender]
                } |> workflow 

        | ContainerNotify(message,except) ->
            let targets = content |> Seq.except except |> Seq.toArray
            for target in targets do target <! Notify(message)

        | ContainerLook(who) ->                                       
            async {
                let! names = aggregateNames childFactory (content |> Seq.except [who])
                who <! Notify(Message("You see {0}",[names]))
            } |> workflow

        | Look -> container <! ContainerLook(self)
        | Say(message) -> 
            container <! ContainerNotify(Message("{0} says {1}",[name;message]), [self])
            notify(Message("You say {0}",[message]))

        | FindByName(nameToFind, except) -> 
            async {
                let! res = getObjectNames childFactory (content |> Seq.except except )
                let cleanName = nameToFind |> RemovePrefix
                let firstMatch = res |> Seq.tryFind (fun (a,n) -> n.ToLowerInvariant().Contains(cleanName)) 
                match firstMatch with
                | Some(a,n) -> sender <! NameFound(a,n)
                | None -> sender <! NameNotFound

            } |> workflow

        | Take(nameOfObject) -> 
            async {
                let! findResult = container.Ask<FindByNameResult>(FindByName(nameOfObject,[self]),TimeSpan.FromSeconds(1.0))
                match findResult with
                | NameFound(item,name) -> 
                    item <! SetContainer(self)
                    notify(Message("You take {0}",[name]))
                | NameNotFound -> notify(Message("Could not find {0}",[nameOfObject]))

            } |> workflow
 
        | o -> failwith ("unhandled message" + o.ToString())
        return! loop()
    }
    loop()

