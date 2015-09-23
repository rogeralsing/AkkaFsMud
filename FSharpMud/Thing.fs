﻿module Thing
open Akka.Actor
open Akka.FSharp
open System;
open System.Collections.Generic
open System.Threading.Tasks
open Messages
open Utils
open Patterns

type ThingState = {container:IActorRef; output: IActorRef }

let thing (name:string) (mailbox: Actor<ThingMessage>)  = 
    let childFactory : IActorRefFactory = mailbox.Context :> IActorRefFactory

    let findContentByName content except nameToFind = 
        let res = content |> Seq.filter (fun no -> not (except |> Seq.contains no.ref) )
        let cleanName = nameToFind |> RemovePrefix
        let firstMatch = res |> Seq.tryFind (fun no -> no.name.ToLowerInvariant().Contains(cleanName)) 
        firstMatch

    //TODO: make immutable and apart of state. ActorRef missing structural comp atm
    let content = new HashSet<NamedObject>()
    let objectsYouSee = new HashSet<NamedObject>()
    let self = mailbox.Self
    let notify message = self <! Notify(message)
    let containerNotify message except = self <! ContainerNotify(message,except)
    let namedSelf = {name=name;ref = self}
    let rec loop(state: ThingState) = actor {        
        let! message = mailbox.Receive()
        match message with
        // Container actions
        | SetContainer(newContainer) ->
            state.container <! RemoveContent(namedSelf)
            newContainer <! AddContent(namedSelf)
            return! loop {state with container = newContainer}

        | AddContent(who) -> 
            if content.Add(who) then 
                self <! DescribeContainer(who)
                for no in content |> Seq.except [who] do
                    no.ref <! AddedContent(who)                

        | RemoveContent(who) -> 
            if content.Remove(who) then 
                for no in content do
                    no.ref <! RemovedContent(who)
        | ContainerNotify(message,except) ->
            let targets = content |> Seq.map(fun no -> no.ref) |> Seq.except except |> Seq.toArray
            for target in targets do target <! Notify(message)

        | DescribeContainer(who) ->         
            let copy = content |> Seq.toList //TODO: contents is currently mutable
            who.ref <! ContainerContent(copy)    

        | AddedContent(who) -> 
            notify(Message("{0} appears",[who.name]))
            objectsYouSee.Add(who) |> ignore

        | RemovedContent(who) -> 
            notify(Message("{0} disappears",[who.name]))
            objectsYouSee.Remove(who) |> ignore

        | ContainerContent(containerContent) -> 
            let names = joinStrings (containerContent |> List.except [namedSelf] |> List.map(fun no -> no.name) |> List.toArray)
            objectsYouSee.Clear()
            for no in containerContent do objectsYouSee.Add (no) |> ignore
            notify(Message("You see {0}",[names]))    

        //Player / NPC Actions
        | Look -> state.container <! DescribeContainer(namedSelf)
        | Say(message) -> 
            state.container <! ContainerNotify(Message("{0} says {1}",[name;message]), [self])
            notify(Message("You say {0}",[message]))
        | Take(nameOfObject) -> 
            let findResult = findContentByName objectsYouSee [] nameOfObject
            match findResult with
            | Some(no) -> 
                no.ref <! SetContainer(self)
                notify(Message("You take {0}",[no.name]))
            | None -> notify(Message("Could not find {0}",[nameOfObject]))

        | Drop(nameOfObject) -> 
            let findResult = findContentByName content [] nameOfObject
            match findResult with
            | Some(no) -> 
                no.ref <! SetContainer(state.container)
                notify(Message("You drop {0}",[no.name]))
            | None -> notify(Message("Could not find {0}",[nameOfObject]))
        | Put(nameOfTarget,nameOfContainer) -> 
            let findResult = findContentByName (objectsYouSee |> Seq.append content) [] nameOfTarget
            match findResult with
            | Some(no1) -> 
                let findResult2 = findContentByName (objectsYouSee |> Seq.append content) [] nameOfContainer
                match findResult2 with
                | Some(no2) -> 
                    no1.ref <! SetContainer(no2.ref)
                    notify(Message("You put {0} in {1}",[no1.name; no2.name]))

                | None -> notify(Message("Could not find {0}",[nameOfContainer]))

            | None -> notify(Message("Could not find {0}",[nameOfTarget]))

        | Inventory ->
            let names = joinStrings (content |> Seq.map (fun no -> no.name) |> Seq.toArray)
            self <! Notify(Message("You have {0}",[names]))

        //output stream actions
        | Notify(message) -> state.output <! message
        | SetOutput(newOutput) -> return! loop {state with output = newOutput}   
        | o -> failwith ("unhandled message" + o.ToString())
        return! loop state
    }
    let nobody = ActorRefs.Nobody :> IActorRef
    loop {container = nobody ; output = nobody}

