module Thing

open Akka.Actor
open Akka.FSharp
open Messages
open Utils

type ThingState = 
    { container : NamedObject
      output : IActorRef
      objectsYouHave : Set<NamedObject>
      objectsYouSee : Set<NamedObject>
      exitsYouHave : Set<NamedObject>
      exitsYouSee : Set<NamedObject> }

let notify target format (args : List<obj>) = target <! Notify(Message(format, args))

let thing (name : string) (mailbox : Actor<ThingMessage>) = 
    let self = mailbox.Self
    
    let namedSelf = 
        { name = name
          ref = self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            // Container actions
            | AddExit(exit) -> return! loop { state with exitsYouHave = state.exitsYouHave.Add(exit) }
            | SetContainerByActorRef(newContainer) -> newContainer <! AddContent(namedSelf)
            | AddContent(who) -> 
                for no in state.objectsYouHave do
                    no.ref <! AddedContent(who)
                let newObjectsYouHave = state.objectsYouHave.Add who
                who.ref <! NewContainerAssigned(namedSelf, newObjectsYouHave, state.exitsYouHave)
                return! loop { state with objectsYouHave = newObjectsYouHave }
            | RemoveContent(who, container) -> 
                for no in state.objectsYouHave |> Seq.except [ who ] do
                    no.ref <! RemovedContent(who, container)
                return! loop { state with objectsYouHave = state.objectsYouHave.Remove who }
            | ContainerNotify(message, except) -> 
                let targets = 
                    state.objectsYouHave
                    |> Seq.map (fun no -> no.ref)
                    |> Seq.except except
                for target in targets do
                    target <! Notify(message)
            | AddedContent(who) -> 
                notify self "{0} appears" [ who.name ]
                return! loop { state with objectsYouSee = state.objectsYouSee.Add who }
            | RemovedContent(who, container) -> 
                if container <> namedSelf && not (state.objectsYouHave.Contains container) then 
                    notify self "{0} disappears into {1}" [ who.name; container.name ]
                return! loop { state with objectsYouSee = state.objectsYouSee.Remove who }
            | NewContainerAssigned(container, containerContent, exits) -> 
                self <! Look
                state.container.ref <! RemoveContent(namedSelf, container)
                return! loop { state with container = container
                                          objectsYouSee = containerContent
                                          exitsYouSee = exits }
            //Player / NPC Actions
            | Look -> 
                let objectNames = 
                    joinStrings (state.objectsYouSee
                                 |> Seq.except [ namedSelf ]
                                 |> Seq.map (fun no -> no.name)
                                 |> Seq.toArray)
                
                let exitNames = 
                    joinStrings (state.exitsYouSee
                                 |> Seq.map (fun no -> no.name)
                                 |> Seq.toArray)
                
                notify self "You are in {0}" [ state.container.name ]
                notify self "You see {0}" [ objectNames ]
                notify self "Exits are: {0}" [ exitNames ]
            | Say(message) -> 
                state.container.ref <! ContainerNotify(Message("{0} says {1}", [ name; message ]), [ self ])
                notify self "You say {0}" [ message ]
            | Take(nameOfObject) -> 
                let findResult = findObjectByName state.objectsYouSee nameOfObject
                match findResult with
                | Some(no) -> 
                    self <! AddContent(no)
                    notify self "You take {0}" [ no.name ]
                | None -> notify self "Could not find {0}" [ nameOfObject ]
            | Drop(nameOfObject) -> 
                let findResult = findObjectByName state.objectsYouHave nameOfObject
                match findResult with
                | Some(no) -> 
                    state.container.ref <! AddContent(no)
                    notify self "You drop {0}" [ no.name ]
                | None -> notify self "Could not find {0}" [ nameOfObject ]
            | Put(nameOfTarget, nameOfContainer) -> 
                let targets = 
                    (state.objectsYouSee
                     |> Seq.append state.objectsYouHave
                     |> Seq.except [ namedSelf ])
                
                let findResult = findObjectByName targets nameOfTarget
                match findResult with
                | Some(no1) -> 
                    let findResult2 = findObjectByName targets nameOfContainer
                    match findResult2 with
                    | Some(no2) -> 
                        no2.ref <! AddContent(no1)
                        notify self "You put {0} in {1}" [ no1.name; no2.name ]
                    | None -> notify self "Could not find {0}" [ nameOfContainer ]
                | None -> notify self "Could not find {0}" [ nameOfTarget ]
            | Inventory -> 
                let names = 
                    joinStrings (state.objectsYouHave
                                 |> Seq.map (fun no -> no.name)
                                 |> Seq.toArray)
                self <! Notify(Message("You have {0}", [ names ]))
            | Go(direction) -> 
                let exit = findObjectByName state.exitsYouSee direction
                match exit with
                | Some(no) -> no.ref <! AddContent(namedSelf)
                | None -> notify self "You can not go {0}" [ direction ]
            //output stream actions
            | Notify(message) -> state.output <! message
            | SetOutput(newOutput) -> return! loop { state with output = newOutput }
            | Where -> notify self "You are in {0}" [ state.container.name ]
            | Fight(nameOfTarget) -> failwith "Not implemented yet"
            | SetTarget(_) -> failwith "Not implemented yet"
            | AttackCurrentTarget -> failwith "Not implemented yet"
            | TakeDamage(attacker, damage) -> failwith "Not implemented yet"
            | NotifyCombatStatus -> failwith "Not implemented yet"
            | Died -> failwith "Not implemented yet"
            return! loop state
        }
    
    let emptySet = Set.empty<NamedObject>
    let nobody = ActorRefs.Nobody :> IActorRef
    
    let namedNobody = 
        { name = "void"
          ref = nobody }
    loop { container = namedNobody
           output = nobody
           objectsYouHave = emptySet
           objectsYouSee = emptySet
           exitsYouHave = emptySet
           exitsYouSee = emptySet }
