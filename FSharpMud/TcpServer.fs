module ConnectionHandler
open Akka.Actor
open Akka.IO
open Akka.FSharp
open System.Net
open Actors
open Messages
open System.Text
open AnsiSupport
open InputHandler
open System

let receiveInput (inputBuffer:StringBuilder) (received:Tcp.Received)=
    let text = Encoding.ASCII.GetString(received.Data.ToArray());
    inputBuffer.Append(text) |> ignore

    let all = inputBuffer.ToString()
    match all.IndexOf('\r') with
    | enter when enter >= 0 ->
        let textToProcess = all.Substring(0,enter)
        inputBuffer.Remove(0,enter+2) |> ignore

        textToProcess
        |> Seq.fold (fun acc c -> if c = '\b' then acc |> List.tail else c::acc) []
        |> List.rev
        |> List.toArray
        |> String
        |> Some
    | _ ->
        None

let write target (text:string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(text)  
    let byteString = ByteString.Create(bytes,0,bytes.Length)
    target <! (Tcp.Write.Create(byteString))

let playerHandler (startRoom:IActorRef) (remote:EndPoint) (connection:IActorRef) (mailbox : Actor<obj>) = 
    mailbox.Context.Watch connection |> ignore

    let close () = 
        printfn "Stopped, remote connection [%A] closed" remote
        mailbox.Context.Stop mailbox.Self
   
    let inputBuffer = new StringBuilder()    
    write connection "Welcom to Akka FS MUD\r\n"
    write connection "Please enter your name\r\n"
    let rec play player = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | :? Message as msg -> write connection (formatAnsi msg.format msg.args)
            | :? Tcp.Received as received ->                 
                match receiveInput inputBuffer received with 
                | Some(command) -> handleInput player command
                | None -> ()

            | :? Tcp.ConnectionClosed | :? Terminated -> close ()
            | _ -> ()
            return! play player
        }

    let rec login() = 
        actor { 
            let! message = mailbox.Receive()
            match message with         
            | :? Tcp.Received as received -> 
                match receiveInput inputBuffer received with 
                | Some(name) ->
                    write connection (toAnsi("You will be known as " + name.yellow+ "\r\n"))
                    let player = spawn mailbox.Context.System null (living name)
                    player <! SetOutput(mailbox.Self)
                    player <! SetContainerByActorRef(startRoom)
                    player <! Look
                    inputBuffer.Clear() |> ignore //TODO: this is ugly
                    return! play player
                | None -> ()

            | :? Tcp.ConnectionClosed | :? Terminated -> close ()
            | _ -> ()
            return! login()
        }
    login()

let mudService (startRoom:IActorRef) (endpoint:IPEndPoint) (mailbox : Actor<obj>) = 
    let manager = mailbox.Context.System.Tcp()
    manager <! (new Tcp.Bind(mailbox.Self, endpoint));
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | :? Tcp.Connected as connected -> 
                printfn "Remote address %A connected" connected.RemoteAddress;
                let handler = spawn mailbox.Context.System null (playerHandler startRoom (connected.RemoteAddress) (mailbox.Sender()))
                mailbox.Sender() <! new Tcp.Register(handler)
            | _ -> ()
            return! loop()
        }
    loop()