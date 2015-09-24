module OutputHandler
open AnsiSupport
open Messages
open Akka.FSharp
open System

let outputHandler (mailbox : Actor<Message>) = 
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | Message(format, args) -> 
                let f = removeAnsi format
                let res = removeAnsi (String.Format(f, args |> List.toArray))
                Console.WriteLine(res)
            return! loop()
        }
    loop()
