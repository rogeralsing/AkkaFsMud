module ConsoleOutput
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
            let f = stripAnsi message.format
            let res = stripAnsi (String.Format(f, message.args |> List.toArray))
            Console.WriteLine(res)
            return! loop()
        }
    loop()
