module OutputHandler
open AnsiSupport
open Messages
open Akka.FSharp
open System

/// <summary>
/// 
/// </summary>
/// <param name="mailbox"></param>
let outputHandler (mailbox : Actor<Message>) = 
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | Message(format, args) -> 
                let f = stripAnsi format
                let res = stripAnsi (String.Format(f, args |> List.toArray))
                Console.WriteLine(res)
            return! loop()
        }
    loop()
