module ConsoleOutput
open AnsiSupport
open Messages
open Akka.FSharp
open System

let consoleOutputHandler (mailbox : Actor<Message>) = 
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            let f = stripAnsi message.format
            let res = stripAnsi (String.Format(f, message.args |> List.toArray))
            Console.WriteLine(res)
            return! loop()
        }
    loop()
