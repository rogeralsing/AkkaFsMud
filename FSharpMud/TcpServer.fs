module ConnectionHandler
open Akka.Actor
open Akka.IO
open Akka.FSharp
open System.Net
open Thing
open Messages
open System.Text
open AnsiSupport
open InputHandler

let connectionHandler (startRoom:IActorRef) (remote:EndPoint) (connection:IActorRef) (mailbox : Actor<obj>) = 
    mailbox.Context.Watch connection |> ignore
    let player = spawn mailbox.Context.System null (thing "player")
    let sb = new StringBuilder()
    player <! SetOutput(mailbox.Self)
    player <! SetContainerByActorRef(startRoom)
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | :? Message as msg -> 
                match msg with
                | Message(format,args) ->
                    let f = formatAnsi format
                    let str = formatAnsi (System.String.Format(f,args |> List.toArray) + "\r\n")
                    let bytes = System.Text.Encoding.UTF8.GetBytes(str)  
                    let byteString = ByteString.Create(bytes,0,bytes.Length)
                    connection <! (Tcp.Write.Create(byteString))
            | :? Tcp.Received as received -> 
                let text = System.Text.Encoding.UTF8.GetString(received.Data.ToArray());
                sb.Append(text) |> ignore
                let all = sb.ToString()
                let enter = all.IndexOf('\r')
                if enter >= 0 then
                    let command = all.Substring(0,enter)
                    sb.Remove(0,enter+2) |> ignore
                    handleInput player command

            | :? Tcp.ConnectionClosed -> 
                printfn "Stopped, remote connection [%A] closed" remote
                mailbox.Context.Stop mailbox.Self
            | :? Terminated -> 
                printfn "Stopped, remote connection [%A] died" remote
                mailbox.Context.Stop mailbox.Self
            | _ -> ()
            return! loop()
        }
    loop()

let mudService (startRoom:IActorRef) (endpoint:IPEndPoint) (mailbox : Actor<obj>) = 
    let manager = mailbox.Context.System.Tcp()
    manager <! (new Tcp.Bind(mailbox.Self, endpoint));
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | :? Tcp.Connected as connected -> 
                printfn "Remote address %A connected" connected.RemoteAddress;
                let handler = spawn mailbox.Context.System null (connectionHandler startRoom (connected.RemoteAddress) (mailbox.Sender()))
                mailbox.Sender() <! new Tcp.Register(handler)
            | _ -> ()
            return! loop()
        }
    loop()