module OutputHandler
open Messages
open Akka.FSharp
open System

let outputHandler (mailbox: Actor<Message>) =
    let rec loop() = actor {        
        let! message = mailbox.Receive()
        match message with
        | Message(format,args) ->
            let res = String.Format(format,args |> List.toArray)
            Console.WriteLine(res)
        return! loop()
    }
    loop()