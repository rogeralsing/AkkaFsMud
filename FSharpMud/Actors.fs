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
    let namedSelf = 
        { name = name
          ref = mailbox.Self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! m = mailbox.Receive()
            match m with
            | :? ContainerMessage as message -> return! containerHandler message namedSelf loop state
            | :? ContainedMessages as message -> return! containedHandler message namedSelf loop state
            | :? NotifyMessages as message -> return! notifyHandler message namedSelf loop state
            | :? ThingMessage as message -> return! thingHandler message namedSelf loop state
            | unhandled -> mailbox.Unhandled unhandled
        }
    
    loop emptyState

let living (name : string) (mailbox : Actor<obj>) = 
    let namedSelf = 
        { name = name
          ref = mailbox.Self }
    
    let rec loop (state : ThingState) = 
        actor { 
            let! m = mailbox.Receive()
            match m with
            | :? ContainerMessage as message -> return! containerHandler message namedSelf loop state
            | :? ContainedMessages as message -> return! containedHandler message namedSelf loop state
            | :? NotifyMessages as message -> return! notifyHandler message namedSelf loop state
            | :? ThingMessage as message -> return! thingHandler message namedSelf loop state
            | unhandled -> mailbox.Unhandled unhandled
        }
    
    loop emptyState
