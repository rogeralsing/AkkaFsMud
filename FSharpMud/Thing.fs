module Thing
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
        | SetContainer(newContainer) ->
            state.container <! ContainerRemove(namedSelf)
            newContainer <! ContainerAdd(namedSelf)
            return! loop {state with container = newContainer}

        | ContainerAdd(who) -> 
            if content.Add(who) then 
                self <! ContainerLook(who)
                for no in content |> Seq.except [who] do
                    no.ref <! ContainerAdded(who)                

        | ContainerRemove(who) -> 
            if content.Remove(who) then 
                for no in content do
                    no.ref <! ContainerRemoved(who)

        | Notify(message) -> state.output <! message
        | SetOutput(newOutput) -> return! loop {state with output = newOutput}   
        | ContainerNotify(message,except) ->
            let targets = content |> Seq.map(fun no -> no.ref) |> Seq.except except |> Seq.toArray
            for target in targets do target <! Notify(message)

        | ContainerLook(who) ->         
            let copy = content |> Seq.toList //TODO: contents is currently mutable
            who.ref <! ContainerContent(copy)                            

        | Look -> state.container <! ContainerLook(namedSelf)
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

        | Inventory ->
            let names = joinStrings (content |> Seq.map (fun no -> no.name) |> Seq.toArray)
            self <! Notify(Message("You have {0}",[names]))

        | ContainerAdded(who) -> 
            notify(Message("{0} appears",[who.name]))
            objectsYouSee.Add(who) |> ignore

        | ContainerRemoved(who) -> 
            notify(Message("{0} disappears",[who.name]))
            objectsYouSee.Remove(who) |> ignore

        | ContainerContent(containerContent) -> 
            let names = joinStrings (containerContent |> List.except [namedSelf] |> List.map(fun no -> no.name) |> List.toArray)
            objectsYouSee.Clear()
            for no in containerContent do objectsYouSee.Add (no) |> ignore
            notify(Message("You see {0}",[names]))
        | o -> failwith ("unhandled message" + o.ToString())
        return! loop state
    }
    let nobody = ActorRefs.Nobody :> IActorRef
    loop {container = nobody ; output = nobody}

