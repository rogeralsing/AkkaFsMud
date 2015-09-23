module Thing

open Akka.Actor
open Akka.FSharp
open System
open System.Collections.Generic
open System.Threading.Tasks
open Messages
open Utils

type ThingState = 
    { container : IActorRef
      output : IActorRef
      objectsYouHave : list<NamedObject>
      objectsYouSee : list<NamedObject> }

let thing (name : string) (mailbox : Actor<ThingMessage>) = 
    let childFactory : IActorRefFactory = mailbox.Context :> IActorRefFactory
    
    let findContentByName objectsYouHave except nameToFind = 
        let res = objectsYouHave |> Seq.filter (fun no -> not (except |> Seq.contains no.ref))
        let cleanName = nameToFind |> RemovePrefix
        let firstMatch = res |> Seq.tryFind (fun no -> no.name.ToLowerInvariant().Contains(cleanName))
        firstMatch
    
    let self = mailbox.Self
    let notify message = self <! Notify(message)
    let containerNotify message except = self <! ContainerNotify(message, except)
    
    let namedSelf = 
        { name = name
          ref = self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            // Container actions
            | SetContainer(newContainer) -> 
                state.container <! RemoveContent(namedSelf)
                newContainer <! AddContent(namedSelf)
                return! loop { state with container = newContainer }
            | AddContent(who) -> 
                self <! DescribeContainer(who)
                for no in state.objectsYouHave |> Seq.except [ who ] do
                    no.ref <! AddedContent(who)
                return! loop { state with objectsYouHave = who :: state.objectsYouHave }
            | RemoveContent(who) -> 
                for no in state.objectsYouHave do
                    no.ref <! RemovedContent(who)
                return! loop { state with objectsYouHave = state.objectsYouHave |> listRemove who }
            | ContainerNotify(message, except) -> 
                let targets = 
                    state.objectsYouHave
                    |> Seq.map (fun no -> no.ref)
                    |> Seq.except except
                for target in targets do
                    target <! Notify(message)
            | DescribeContainer(who) -> 
                let copy = state.objectsYouHave
                who.ref <! ContainerContent(copy)
            | AddedContent(who) -> 
                notify (Message("{0} appears", [ who.name ]))
                return! loop { state with objectsYouSee = who :: state.objectsYouSee }
            | RemovedContent(who) -> 
                notify (Message("{0} disappears", [ who.name ]))
                return! loop { state with objectsYouSee = state.objectsYouSee |> listRemove who }
            | ContainerContent(containerContent) -> 
                let names = 
                    joinStrings (containerContent
                                 |> List.except [ namedSelf ]
                                 |> List.map (fun no -> no.name)
                                 |> List.toArray)
                notify (Message("You see {0}", [ names ]))
                return! loop { state with objectsYouSee = containerContent }
            //Player / NPC Actions
            | Look -> state.container <! DescribeContainer(namedSelf)
            | Say(message) -> 
                state.container <! ContainerNotify(Message("{0} says {1}", [ name; message ]), [ self ])
                notify (Message("You say {0}", [ message ]))
            | Take(nameOfObject) -> 
                let findResult = findContentByName state.objectsYouSee [] nameOfObject
                match findResult with
                | Some(no) -> 
                    no.ref <! SetContainer(self)
                    notify (Message("You take {0}", [ no.name ]))
                | None -> notify (Message("Could not find {0}", [ nameOfObject ]))
            | Drop(nameOfObject) -> 
                let findResult = findContentByName state.objectsYouHave [] nameOfObject
                match findResult with
                | Some(no) -> 
                    no.ref <! SetContainer(state.container)
                    notify (Message("You drop {0}", [ no.name ]))
                | None -> notify (Message("Could not find {0}", [ nameOfObject ]))
            | Put(nameOfTarget, nameOfContainer) -> 
                let targets = 
                    (state.objectsYouSee
                     |> Seq.append state.objectsYouHave
                     |> Seq.except [ namedSelf ])
                
                let findResult = findContentByName targets [] nameOfTarget
                match findResult with
                | Some(no1) -> 
                    let findResult2 = findContentByName targets [] nameOfContainer
                    match findResult2 with
                    | Some(no2) -> 
                        no1.ref <! SetContainer(no2.ref)
                        notify (Message("You put {0} in {1}", [ no1.name; no2.name ]))
                    | None -> notify (Message("Could not find {0}", [ nameOfContainer ]))
                | None -> notify (Message("Could not find {0}", [ nameOfTarget ]))
            | Inventory -> 
                let names = 
                    joinStrings (state.objectsYouHave
                                 |> Seq.map (fun no -> no.name)
                                 |> Seq.toArray)
                self <! Notify(Message("You have {0}", [ names ]))
            //output stream actions
            | Notify(message) -> state.output <! message
            | SetOutput(newOutput) -> return! loop { state with output = newOutput }
            | o -> failwith ("unhandled message" + o.ToString())
            return! loop state
        }
    
    let emptyList = List.empty<NamedObject>
    let nobody = ActorRefs.Nobody :> IActorRef
    loop { container = nobody
           output = nobody
           objectsYouHave = emptyList
           objectsYouSee = emptyList }
