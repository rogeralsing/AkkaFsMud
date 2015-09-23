module Patterns
open Akka.Actor
open Akka.FSharp
open System;
open System.Collections.Generic
open System.Threading.Tasks
open Messages
open Utils

let aggregator (targets: seq<IActorRef>) (message: obj) (tcs : TaskCompletionSource<(IActorRef * 'a)[]>)(mailbox: Actor<obj>) =

    let count = Seq.length targets
    let results = new Dictionary<IActorRef,'a>()
    let complete res =
        tcs.SetResult(res)
        mailbox.Context.Stop(mailbox.Self)

    for target in targets do target <! message
    
    mailbox.Context.SetReceiveTimeout(Nullable(TimeSpan.FromSeconds(1.0)))

    let rec loop() = actor {                 
        let! message = mailbox.Receive()
        match message with
        | :? ReceiveTimeout ->
            complete([||])
        | :? 'a as msg ->
            results.Add (mailbox.Sender(),msg)
            if (results.Count = count) then
                let arr = results 
                            |> Seq.map (fun kvp -> (kvp.Key, kvp.Value)) 
                            |> Seq.toArray
                complete(arr)

        | _ -> ignore() //TODO: fail?
        return! loop()
    }
    loop()

let aggregate<'a> (actorFactory: IActorRefFactory) (targets: seq<IActorRef>) (message: obj) (timeout: TimeSpan) =
    let count = Seq.length targets
    if (count = 0) then 
        Task.FromResult([||])
    else
        let tcs = new TaskCompletionSource<(IActorRef * 'a)[]>()
        let actor = spawn actorFactory null (aggregator targets message tcs)
        tcs.Task

