module Messages

open Akka.Actor
open Utils

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
            | :? NamedObject as y -> compare x.name y.name
            | _ -> invalidArg "yobj" "cannot compare values of different types"

let findObjectByName objects nameToFind = 
    let cleanName = nameToFind |> removePrefix
    objects |> Seq.tryFind (fun no -> no.name.ToLowerInvariant().Contains(cleanName))

type Message = 
    | Message of format : string * args : list<obj>

type ThingMessage = 
    | SetOutput of IActorRef
    | SetContainerByActorRef of IActorRef
    | AddExit of NamedObject
    | AddContent of NamedObject
    | AddedContent of NamedObject
    | RemoveContent of who : NamedObject * newContaner : NamedObject
    | RemovedContent of who : NamedObject * newContaner : NamedObject
    | NewContainerAssigned of container: NamedObject * content: Set<NamedObject> * exits: Set<NamedObject>
    | Where
    | Inventory
    | Look
    | Go of direction : string
    | Say of message : string
    | Notify of Message
    | ContainerNotify of Message * except : seq<IActorRef>
    | Take of nameOfObject : string
    | Enter of nameOfObject : string
    | Exit
    | ExitContainer of who : NamedObject
    | Drop of nameOfObject : string
    | Put of nameOfObjectToTake : string * nameOfContainer : string
    | Fight of nameOfTarget : string
    | SetTarget of NamedObject
    | AttackCurrentTarget
    | TakeDamage of attacker : NamedObject * damage : int
    | NotifyCombatStatus
    | Died
