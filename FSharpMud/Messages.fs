module Messages

open Akka.Actor

[<CustomEquality; CustomComparison>]
type NamedObject = 
    { name : string
      ref : IActorRef }
    override x.Equals(yobj) =
        match yobj with
        | :? NamedObject as y -> (x.name = y.name && x.ref = y.ref)
        | _ -> false

    override x.GetHashCode() = hash x.name
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? NamedObject as y -> compare x.name  y.name 
          | _ -> invalidArg "yobj" "cannot compare values of different types"

type Message = 
    | Message of format : string * args : list<obj>

type ThingMessage = 
    | SetOutput of IActorRef
    | SetContainer of IActorRef
    | AddContent of NamedObject
    | AddedContent of NamedObject
    | RemoveContent of NamedObject
    | RemovedContent of NamedObject
    | ContainerContent of Set<NamedObject>
    | Where
    | Inventory
    | Look
    | Say of message : string
    | Notify of Message
    | ContainerNotify of Message * except : seq<IActorRef>
    | Take of nameOfObject : string
    | Drop of nameOfObject : string
    | Put of nameOfObjectToTake : string * nameOfContainer : string
    | Fight of nameOfTarget : string
    | SetTarget of NamedObject
    | AttackCurrentTarget
    | TakeDamage of attacker : NamedObject * damage : int
    | NotifyCombatStatus
    | Died
