module Thing

open Akka.Actor
open Akka.FSharp
open Messages
open MessageHandlers
open ActorState

let emptyState = 
    let emptySet = Set.empty<NamedObject>
    let nobody = ActorRefs.Nobody :> IActorRef
    
    let namedNobody = 
        { name = "void"
          ref = nobody }
    { container = namedNobody
      output = nobody
      objectsYouHave = emptySet
      objectsYouSee = emptySet
      exitsYouHave = emptySet
      exitsYouSee = emptySet }

let container (name : string) (mailbox : Actor<obj>) = 
    let self = 
        { name = name
          ref = mailbox.Self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! m = mailbox.Receive()
            match m with
            | :? ContainerMessages as message -> return! containerHandler message self loop state
            | :? ContainedMessages as message -> return! containedHandler message self loop state
            | _ -> ()
            return! loop state
        }
    
    loop emptyState

let living (name : string) (mailbox : Actor<obj>) = 
    let self = 
        { name = name
          ref = mailbox.Self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! m = mailbox.Receive()
            match m with
            | :? ContainerMessages as message -> return! containerHandler message self loop state
            | :? ContainedMessages as message -> return! containedHandler message self loop state
            | :? NotifyMessages as message -> return! notifyHandler message self loop state
            | :? AgentMessages as message -> return! agentHandler message self loop state
            | _ -> ()
            return! loop state
        }
    
    loop emptyState
