module MessageHandlers
open Akka.FSharp
open Messages
open AnsiSupport
open Utils
open ActorState

let notify target format (args : List<obj>) = target <! Notify(Message(format, args))
let notifyContainer target format (args : List<obj>) except = target <! ContainerNotify(Message(format, args), except)

let containedHandler message (namedSelf : NamedObject) loop (state : ThingState) = 
    actor { 
        match message with
        | SetContainerByActorRef(newContainer) -> newContainer <! AddContent(namedSelf)
        | AddedContent(who) -> return! loop { state with objectsYouSee = state.objectsYouSee.Add who }
        | RemovedContent(who, _) -> return! loop { state with objectsYouSee = state.objectsYouSee.Remove who }
        | NewContainerAssigned(container, containerContent, exits) -> 
            namedSelf.ref <! Look
            state.container.ref <! RemoveContent(namedSelf, container)
            return! loop { state with container = container
                                      objectsYouSee = containerContent
                                      exitsYouSee = exits }
        return! loop state
    }

let notifyHandler message (namedSelf : NamedObject) loop (state : ThingState) = 
    actor { 
        match message with
        | Notify(message) -> state.output <! message
        | SetOutput(newOutput) -> return! loop { state with output = newOutput }
        return! loop state
    }

let containerHandler message (namedSelf : NamedObject) loop (state : ThingState) = 
    actor { 
        match message with
        | AddExit(exit) -> return! loop { state with exitsYouHave = state.exitsYouHave.Add(exit) }
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
            notifyContainer from.ref "{0} disappears into {1}" [ who.name.yellow; namedSelf.name.yellow ] [ who.ref ]
            notifyContainer namedSelf.ref "{0} appears from {1}" [ who.name.yellow; from.name.yellow ] [ who.ref ]
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
        | ExitContainer(who, _) -> state.container.ref <! EnterRoom(who, namedSelf)
        return! loop state
    }

let thingHandler message (namedSelf : NamedObject) loop (state : ThingState) = 
    actor { 
        match message with
        | Look -> 
            let objectNames = 
                joinStrings (state.objectsYouSee
                             |> Seq.except [ namedSelf ]
                             |> Seq.map (fun no -> no.name.yellow))
            
            let exitNames = joinStrings (state.exitsYouSee |> Seq.map (fun no -> no.name.yellow))
            notify namedSelf.ref "You are in {0}" [ state.container.name.yellow ]
            notify namedSelf.ref ("You see " + objectNames) []
            notify namedSelf.ref ("Exits are: " + exitNames) []
        | Say(message) -> 
            notifyContainer state.container.ref "{0} says {1}" [ namedSelf.name.yellow; message.green ] 
                [ namedSelf.ref ]
            notify namedSelf.ref "You say {0}" [ message.green ]
        | Yell(message) -> 
            notifyContainer state.container.ref "{0} yells {1}" [ namedSelf.name.yellow; message.red ] [ namedSelf.ref ]
            notify namedSelf.ref "You yell {0}" [ message.red ]
        | Take(nameOfObject) -> 
            let findResult = findObjectByName state.objectsYouSee nameOfObject
            match findResult with
            | Some(no) -> 
                notifyContainer state.container.ref "{0} takes {1}" [ namedSelf.name.yellow; no.name.yellow ] 
                    [ namedSelf.ref ]
                namedSelf.ref <! AddContent(no)
                notify namedSelf.ref "You take {0}" [ no.name.yellow ]
            | None -> notify namedSelf.ref "Could not find {0}" [ nameOfObject ]
        | Enter(nameOfObject) -> 
            let findResult = findObjectByName state.objectsYouSee nameOfObject
            match findResult with
            | Some(no) -> 
                notifyContainer state.container.ref "{0} enters {1}" [ namedSelf.name.yellow; no.name.yellow ] 
                    [ namedSelf.ref ]
                no.ref <! AddContent(namedSelf)
                notify namedSelf.ref "You enter {0}" [ no.name.yellow ]
            | None -> notify namedSelf.ref "Could not find {0}" [ nameOfObject.yellow ]
        | Exit -> state.container.ref <! ExitContainer(namedSelf, state.container)
        | Drop(nameOfObject) -> 
            let findResult = findObjectByName state.objectsYouHave nameOfObject
            match findResult with
            | Some(no) -> 
                notifyContainer state.container.ref "{0} drops {1}" [ namedSelf.name.yellow; no.name.yellow ] 
                    [ namedSelf.ref ]
                state.container.ref <! AddContent(no)
                notify namedSelf.ref "You drop {0}" [ no.name.yellow ]
            | None -> notify namedSelf.ref "Could not find {0}" [ nameOfObject.yellow ]
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
                    notify namedSelf.ref "You put {0} in {1}" [ no1.name.yellow; no2.name.yellow ]
                | None -> notify namedSelf.ref "Could not find {0}" [ nameOfContainer.yellow ]
            | None -> notify namedSelf.ref "Could not find {0}" [ nameOfTarget.yellow ]
        | Inventory -> 
            let names = joinStrings (state.objectsYouHave |> Seq.map (fun no -> no.name.yellow))
            namedSelf.ref <! Notify(Message("You have " + names, []))
            notifyContainer state.container.ref "{0} checks his inventory" [ namedSelf.name.yellow ] [ namedSelf.ref ]
        | Go(direction) -> 
            let exit = findObjectByName state.exitsYouSee direction
            match exit with
            | Some(no) -> no.ref <! EnterRoom(namedSelf, state.container)
            | None -> notify namedSelf.ref "You can not go {0}" [ direction ]
        return! loop state
    }
