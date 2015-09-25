module ActorState
open Akka.Actor
open Utils

[<CustomEquality; CustomComparison>]
type NamedObject = 
    { name : string
      ref : IActorRef }
    
    override x.Equals(yobj) = 
        match yobj with
        | :? NamedObject as y -> (x.name = y.name && x.ref.Path = y.ref.Path)
        | _ -> false
    
    override x.GetHashCode() = hash x.name
    interface System.IComparable with
        member x.CompareTo yobj = 
            match yobj with
            | :? NamedObject as y -> 
                compare (x.name + x.ref.Path.ToSerializationFormat()) (y.name + y.ref.Path.ToSerializationFormat())
            | _ -> invalidArg "yobj" "cannot compare values of different types"

let findObjectByName objects nameToFind = 
    let cleanName = nameToFind |> removePrefix
    objects |> Seq.tryFind (fun no -> no.name.ToLowerInvariant().Contains(cleanName))

type ThingState = 
    { container : NamedObject
      output : IActorRef
      objectsYouHave : Set<NamedObject>
      objectsYouSee : Set<NamedObject>
      exitsYouHave : Set<NamedObject>
      exitsYouSee : Set<NamedObject> }