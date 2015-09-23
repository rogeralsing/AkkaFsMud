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
    let self = mailbox.Self
    let notify message = self <! Notify(message)
    let containerNotify message except = self <! ContainerNotify(message,except)
    let namedSelf = {name=name;ref = self}
    let rec loop(state: ThingState) = actor {        
        let! message = mailbox.Receive()
        match message with
        | GetName(sender) -> sender <! name
        | SetContainer(newContainer) ->
            state.container <! ContainerRemove(namedSelf,self)
            newContainer <! ContainerAdd(namedSelf,self)
            return! loop {state with container = newContainer}

        | ContainerAdd(who,sender) -> if content.Add(who) then containerNotify (Message ("{0} appears",[who.name])) [who.ref; sender]
        | ContainerRemove(who,sender) -> if content.Remove(who) then containerNotify (Message ("{0} disappears",[who.name])) [who.ref; sender]
        | Notify(message) -> state.output <! message
        | SetOutput(newOutput) -> return! loop {state with output = newOutput}   
        | ContainerNotify(message,except) ->
            let targets = content |> Seq.map(fun no -> no.ref) |> Seq.except except |> Seq.toArray
            for target in targets do target <! Notify(message)

        | ContainerLook(who) ->         
            let names = joinStrings (content |> Seq.except [who] |> Seq.map (fun no -> no.name) |> Seq.toArray )
            who.ref <! Notify(Message("You have {0}",[names]))                              

        | Look -> state.container <! ContainerLook(namedSelf)
        | Say(message) -> 
            state.container <! ContainerNotify(Message("{0} says {1}",[name;message]), [self])
            notify(Message("You say {0}",[message]))

        | FindByName(nameToFind, except) -> 
            let res = findContentByName content except nameToFind
            //used by an ask operation so we need dynamic sender
            mailbox.Sender() <! res

        | Take(nameOfObject) -> 
            async {
                let! findResult = state.container.Ask<Option<NamedObject>>(FindByName(nameOfObject,[self]),TimeSpan.FromSeconds(1.0))
                match findResult with
                | Some(no) -> 
                    no.ref <! SetContainer(self)
                    notify(Message("You take {0}",[no.name]))
                | None -> notify(Message("Could not find {0}",[nameOfObject]))

            } |> workflow
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

        | o -> failwith ("unhandled message" + o.ToString())
        return! loop state
    }
    let nobody = ActorRefs.Nobody :> IActorRef
    loop {container = nobody ; output = nobody}

