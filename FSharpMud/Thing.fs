module Thing

open Akka.Actor
open Akka.FSharp
open Messages
open Utils
open System
open AnsiSupport

let handleInput player (input : string) = 
    let parts = input.Split([| ' ' |], 2, StringSplitOptions.None)
    let command = parts |> Seq.tryHead
    
    let message = 
        parts
        |> Seq.skip (1)
        |> Seq.tryLast
    match command, message with
    | Some(str), Some(msg) -> 
        match str with
        | "say" | "s" -> player <! Say(msg)
        | "yell" | "y" -> player <! Yell(msg)
        | "take" | "get" | "t" -> player <! Take(msg)
        | "enter" -> player <! Enter(msg)
        | "drop" | "d" -> player <! Drop(msg)
        | "put" when msg.Contains(" in ") -> 
            let parts = msg.Split([| " in " |], 2, StringSplitOptions.RemoveEmptyEntries)
            let target = parts.[0]
            let container = parts.[1]
            player <! Put(target, container)
        | other -> printfn "unknown command %A" other
    | Some(str), None -> 
        match str with
        | "look" | "l" -> player <! Look
        | "exit" -> player <! Exit
        | "inventory" | "inv" -> player <! Inventory
        | "where" -> player <! Where
        | "north" | "n" -> player <! Go("north")
        | "south" | "s" -> player <! Go("south")
        | "east" | "e" -> player <! Go("east")
        | "west" | "w" -> player <! Go("west")
        | other -> printfn "unknown command %A" other
    | _ -> ignore()

type ThingState = 
    { container : NamedObject
      output : IActorRef
      objectsYouHave : Set<NamedObject>
      objectsYouSee : Set<NamedObject>
      exitsYouHave : Set<NamedObject>
      exitsYouSee : Set<NamedObject> }

let notify target format (args : List<obj>) = target <! Notify(Message(format, args))
let notifyContainer target format (args : List<obj>) except = target <! ContainerNotify(Message(format, args),except)

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
            | EnterRoom(who, from) -> 
                for no in state.objectsYouHave do
                    no.ref <! AddedContent(who)
                let newObjectsYouHave = state.objectsYouHave.Add who
                who.ref <! NewContainerAssigned(namedSelf, newObjectsYouHave, state.exitsYouHave)
                notifyContainer from.ref "{0} disappears into {1}" [ who.name.yellow; name.yellow ] [ who.ref ]
                notifyContainer self "{0} appears from {1}" [ who.name.yellow; from.name.yellow ] [ who.ref ]
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
            | AddedContent(who) -> return! loop { state with objectsYouSee = state.objectsYouSee.Add who }
            | RemovedContent(who, _) -> return! loop { state with objectsYouSee = state.objectsYouSee.Remove who }
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
                                 |> Seq.map (fun no -> no.name.yellow))
                
                let exitNames = joinStrings (state.exitsYouSee |> Seq.map (fun no -> no.name.yellow))
                notify self "You are in {0}" [ state.container.name.yellow ]
                notify self ("You see " + objectNames) []
                notify self ("Exits are: " + exitNames) []
            | Say(message) -> 
                notifyContainer state.container.ref "{0} says {1}" [ name.yellow; message.green ] [ self ]
                notify self "You say {0}" [ message.green ]
            | Yell(message) -> 
                notifyContainer state.container.ref "{0} yells {1}" [ name.yellow; message.red ] [ self ]
                notify self "You yell {0}" [ message.red ]
            | Take(nameOfObject) -> 
                let findResult = findObjectByName state.objectsYouSee nameOfObject
                match findResult with
                | Some(no) -> 
                    notifyContainer state.container.ref "{0} takes {1}" [ name.yellow; no.name.yellow ] [ self ]
                    self <! AddContent(no)
                    notify self "You take {0}" [ no.name.yellow ]
                | None -> notify self "Could not find {0}" [ nameOfObject ]
            | Enter(nameOfObject) -> 
                let findResult = findObjectByName state.objectsYouSee nameOfObject
                match findResult with
                | Some(no) -> 
                    notifyContainer state.container.ref "{0} enters {1}" [ name.yellow; no.name.yellow ] [ self ]
                    no.ref <! AddContent(namedSelf)
                    notify self "You enter {0}" [ no.name.yellow ]
                | None -> notify self "Could not find {0}" [ nameOfObject.yellow ]
            | Exit -> state.container.ref <! ExitContainer(namedSelf, state.container)
            | ExitContainer(who, _) -> state.container.ref <! EnterRoom(who, namedSelf)
            | Drop(nameOfObject) -> 
                let findResult = findObjectByName state.objectsYouHave nameOfObject
                match findResult with
                | Some(no) -> 
                    notifyContainer state.container.ref "{0} drops {1}" [ name.yellow; no.name.yellow ] [ self ]
                    state.container.ref <! AddContent(no)
                    notify self "You drop {0}" [ no.name.yellow ]
                | None -> notify self "Could not find {0}" [ nameOfObject.yellow ]
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
                        notify self "You put {0} in {1}" [ no1.name.yellow; no2.name.yellow ]
                    | None -> notify self "Could not find {0}" [ nameOfContainer.yellow ]
                | None -> notify self "Could not find {0}" [ nameOfTarget.yellow ]
            | Inventory -> 
                let names = joinStrings (state.objectsYouHave |> Seq.map (fun no -> no.name.yellow))
                self <! Notify(Message("You have " + names, []))
                notifyContainer state.container.ref "{0} checks his inventory" [ name.yellow ] [ self ]
            | Go(direction) -> 
                let exit = findObjectByName state.exitsYouSee direction
                match exit with
                | Some(no) -> no.ref <! EnterRoom(namedSelf, state.container)
                | None -> notify self "You can not go {0}" [ direction ]
            //output stream actions
            | Notify(message) -> state.output <! message
            | SetOutput(newOutput) -> return! loop { state with output = newOutput }
            | Where -> notify self "You are in {0}" [ state.container.name.yellow ]
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
