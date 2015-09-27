module MessageHandlers

open ActorState
open Akka.FSharp
open AnsiSupport
open Messages
open StringUtils

let notify target format (args : List<obj>) = 
    target <! Notify({ format = format
                       args = args })

let notifyContainer target format (args : List<obj>) except = 
    target <! ContainerNotify({ format = format
                                args = args }, except)

//handles messages for contained objects, objects residing inside a container
let containedHandler message self loop state = 
    actor { 
        match message with
        | SetContainerByActorRef(newContainer) -> newContainer <! AddContent(self, self)
        | AddedContent(who) -> return! loop { state with objectsYouSee = state.objectsYouSee.Add who }
        | RemovedContent(who, _) -> return! loop { state with objectsYouSee = state.objectsYouSee.Remove who }
        | NewContainerAssigned(container, containerContent, exits) -> 
            self.ref <! Look
            state.container.ref <! RemoveContent(self, container)
            return! loop { state with container = container
                                      objectsYouSee = containerContent
                                      exitsYouSee = exits }
        return! loop state
    }

//handle messages for output streams
let notifyHandler message self loop state = 
    actor { 
        match message with
        | Notify(message) -> state.output <! message
        | SetOutput(newOutput) -> return! loop { state with output = newOutput }
        return! loop state
    }

//handles messages for containers
let containerHandler message self allowEnter allowExit loop state = 
    actor { 
        match message with
        | AddExit(exit) -> return! loop { state with exitsYouHave = state.exitsYouHave.Add(exit) }
        | AddContent(what, who) -> 
            state.objectsYouHave |> Seq.iter (fun no -> no.ref <! AddedContent(what))
            let newObjectsYouHave = state.objectsYouHave.Add what
            what.ref <! NewContainerAssigned(self, newObjectsYouHave, state.exitsYouHave)
            return! loop { state with objectsYouHave = newObjectsYouHave }
        | EnterRoom(who, from) -> 
            if allowEnter then 
                state.objectsYouHave |> Seq.iter (fun no -> no.ref <! AddedContent(who))
                let newObjectsYouHave = state.objectsYouHave.Add who
                who.ref <! NewContainerAssigned(self, newObjectsYouHave, state.exitsYouHave)
                notifyContainer from.ref "{0} disappears into {1}" [ who.name.yellow; self.name.yellow ] [ who.ref ]
                notifyContainer self.ref "{0} appears from {1}" [ who.name.yellow; from.name.yellow ] [ who.ref ]
                notify who.ref "You enter {0}" [ self.name.yellow ]
                return! loop { state with objectsYouHave = newObjectsYouHave }
            else notify who.ref "You can not enter {0}" [ self.name.yellow ]
        | RemoveContent(who, container) -> 
            state.objectsYouHave
            |> Seq.except [ who ]
            |> Seq.iter (fun no -> no.ref <! RemovedContent(who, container))
            return! loop { state with objectsYouHave = state.objectsYouHave.Remove who }
        | ContainerNotify(message, except) -> 
            state.objectsYouHave
            |> Seq.map (fun no -> no.ref)
            |> Seq.except except
            |> Seq.iter (fun target -> target <! Notify(message))
        | ExitContainer(who, _) -> 
            if allowExit then state.container.ref <! EnterRoom(who, self)
            else notify who.ref "You can not exit {0}" [ self.name.yellow ]
        return! loop state
    }

//handles messages for containers
let igoreContainerHandler message self loop state = 
    actor { 
        match message with
        | AddContent(what, who) -> notify who.ref "You can not put {0} in {1}" [ what.name; self.name.yellow ]
        | EnterRoom(who, _) -> notify who.ref "You can not enter {0}" [ self.name.yellow ]
        | _ -> ()
        return! loop state
    }

//handles messages for agents that can interact with the world
let agentHandler message self loop state = 
    actor { 
        match message with
        | Look -> 
            let objectNames = 
                state.objectsYouSee
                |> Seq.except [ self ]
                |> Seq.map (fun no -> no.name.yellow)
                |> joinStrings
            
            let exitNames = 
                state.exitsYouSee
                |> Seq.map (fun no -> no.name.yellow)
                |> joinStrings
            
            notify self.ref "You are in {0}" [ state.container.name.yellow ]
            notify self.ref "You see {0}" [ objectNames ]
            notify self.ref "Exits are: {0}" [ exitNames ]
        | Say(message) -> 
            notifyContainer state.container.ref "{0} says {1}" [ self.name.yellow; message.green ] [ self.ref ]
            notify self.ref "You say {0}" [ message.green ]
        | Yell(message) -> 
            notifyContainer state.container.ref "{0} yells {1}" [ self.name.yellow; message.red ] [ self.ref ]
            notify self.ref "You yell {0}" [ message.red ]
        | Take(nameOfObject) -> 
            let findResult = findObjectByName nameOfObject state.objectsYouSee
            match findResult with
            | Some(no) -> 
                notifyContainer state.container.ref "{0} takes {1}" [ self.name.yellow; no.name.yellow ] [ self.ref ]
                self.ref <! AddContent(no, self)
                notify self.ref "You take {0}" [ no.name.yellow ]
                return! loop { state with objectsYouHave = state.objectsYouHave.Add(no) }
            | None -> notify self.ref "Could not find {0}" [ nameOfObject ]
        | Enter(nameOfObject) -> 
            let findResult = findObjectByName nameOfObject state.objectsYouSee
            match findResult with
            | Some(no) -> no.ref <! EnterRoom(self, state.container)
            | None -> notify self.ref "Could not find {0}" [ nameOfObject.yellow ]
        | Exit -> state.container.ref <! ExitContainer(self, state.container)
        | Drop(nameOfObject) -> 
            let findResult = findObjectByName nameOfObject state.objectsYouHave
            match findResult with
            | Some(no) -> 
                notifyContainer state.container.ref "{0} drops {1}" [ self.name.yellow; no.name.yellow ] [ self.ref ]
                state.container.ref <! AddContent(no, self)
                notify self.ref "You drop {0}" [ no.name.yellow ]
            | None -> notify self.ref "Could not find {0}" [ nameOfObject.yellow ]
        | Put(nameOfTarget, nameOfContainer) -> 
            let targets = 
                state.objectsYouSee
                |> Seq.append state.objectsYouHave
                |> Seq.except [ self ]
            
            let findResult = targets |> findObjectByName nameOfTarget
            match findResult with
            | Some(no1) -> 
                let findResult2 = targets |> findObjectByName nameOfContainer
                match findResult2 with
                | Some(no2) -> no2.ref <! AddContent(no1, self)
                | None -> notify self.ref "Could not find {0}" [ nameOfContainer.yellow ]
            | None -> notify self.ref "Could not find {0}" [ nameOfTarget.yellow ]
        | Inventory -> 
            let names = joinStrings (state.objectsYouHave |> Seq.map (fun no -> no.name.yellow))
            notify self.ref "You have {0}" [ names ]
            notifyContainer state.container.ref "{0} checks his inventory" [ self.name.yellow ] [ self.ref ]
        | Go(direction) -> 
            let exit = findObjectByName direction state.exitsYouSee
            match exit with
            | Some(no) -> no.ref <! EnterRoom(self, state.container)
            | None -> notify self.ref "You can not go {0}" [ direction ]
        return! loop state
    }
