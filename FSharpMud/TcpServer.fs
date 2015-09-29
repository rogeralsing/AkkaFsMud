module ConnectionHandler
open Actors
open Akka.Actor
open Akka.FSharp
open Akka.IO
open AnsiSupport
open InputHandler
open Messages
open System
open System.Net
open System.Text

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

type PlayerState =
     | Login
     | Play of player : IActorRef


let playerHandler (startRoom:IActorRef) (remote:EndPoint) (connection:IActorRef) (mailbox : Actor<obj>) = 
    mailbox.Context.Watch connection |> ignore

    let inputBuffer = StringBuilder()  

    let rec loop (state:PlayerState) =
        actor {
            let! message = mailbox.Receive()
            match message with
            | :? Message as msg -> write connection (formatAnsi msg.format msg.args)
            | :? Tcp.ConnectionClosed | :? Terminated -> 
                printfn "Stopped, remote connection [%A] closed" remote
                mailbox.Context.Stop mailbox.Self
            | :? Tcp.Received as received ->
                match receiveInput inputBuffer received with 
                | Some(input) -> 
                    match state with
                    | Login ->
                        write connection (formatAnsi "You will be known as {0}\r\n" [input.yellow])
                        let player = spawn mailbox.Context.System null (living input)
                        player <! SetOutput(mailbox.Self)
                        player <! SetContainerByActorRef(startRoom)
                        player <! Look
                        return! loop (Play(player))
                    | Play(player) -> 
                        handleInput player input

                | None -> ()
            | _ -> ()
               
            return! loop state    
        }

    write connection "Welcome to Akka FS MUD\r\n"
    write connection "Please enter your name\r\n"
    loop Login

let mudService (startRoom:IActorRef) (endpoint:IPEndPoint) (mailbox : Actor<obj>) = 
    let manager = mailbox.Context.System.Tcp()
    manager <! Tcp.Bind(mailbox.Self, endpoint);
    let rec loop() = 
        actor { 
            let! message = mailbox.Receive()
            match message with
            | :? Tcp.Connected as connected -> 
                printfn "Remote address %A connected" connected.RemoteAddress;
                let handler = spawn mailbox.Context.System null (playerHandler startRoom (connected.RemoteAddress) (mailbox.Sender()))
                mailbox.Sender() <! Tcp.Register(handler)
            | _ -> ()
            return! loop()
        }
    loop()