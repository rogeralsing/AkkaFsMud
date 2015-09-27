module AnsiSupport
open System

let ansiColors = 
    [ (// Our reset values turns everything to the default mode
       "{reset}", "\x1B[0m", "Reset")
      (// Style Modifiers (on)
       "{bold}", "\x1B[1m", "Bold")
      ("{italic}", "\x1B[3m", "Italic")
      ("{ul}", "\x1B[4m", "Underline")
      ("{blink}", "\x1B[5m", "Blink")
      ("{blinkf}", "\x1B[6m", "Blink Fast")
      ("{inverse}", "\x1B[7m", "Inverse")
      ("{strike}", "\x1B[9m", "Strikethrough")
      (// Style Modifiers (off)
       "{!bold}", "\x1B[22m", "Bold Off")
      ("{!italic}", "\x1B[23m", "Italic Off")
      ("{!ul}", "\x1B[24m", "Underline Off")
      ("{!blink}", "\x1B[25m", "Blink Off")
      ("{!inverse}", "\x1B[27m", "Inverse Off")
      ("{!strike}", "\x1B[29m", "Strikethrough Off")
      (// Foreground Color
       "{black}", "\x1B[30m", "Foreground black")
      ("{red}", "\x1B[31m", "Foreground red")
      ("{green}", "\x1B[32m", "Foreground green")
      ("{yellow}", "\x1B[33m", "Foreground yellow")
      ("{blue}", "\x1B[34m", "Foreground blue")
      ("{magenta}", "\x1B[35m", "Foreground magenta")
      ("{cyan}", "\x1B[36m", "Foreground cyan")
      ("{white}", "\x1B[37m", "Foreground white")
      (// Background Color
       "{!black}", "\x1B[40m", "Background black")
      ("{!red}", "\x1B[41m", "Background red")
      ("{!green}", "\x1B[42m", "Background green")
      ("{!yellow}", "\x1B[43m", "Background yellow")
      ("{!blue}", "\x1B[44m", "Background blue")
      ("{!magenta}", "\x1B[45m", "Background magenta")
      ("{!cyan}", "\x1B[46m", "Background cyan")
      ("{!white}", "\x1B[47m", "Background white") ]

let toAnsi (input:string) =
    ansiColors 
    |> Seq.fold (fun (acc:string) (name,escape,_) -> acc.Replace(name,escape)) input

let stripAnsi (input:string) =
    ansiColors 
    |> Seq.fold (fun (acc:string) (name,_,_) -> acc.Replace(name,"")) input

let formatAnsi (format:string) (args:List<obj>) =
     let f = toAnsi format
     toAnsi (String.Format(f,args |> List.toArray) + "\r\n")

type String with
    member this.yellow =
        "{yellow}" + this + "{reset}"
    member this.green =
        "{green}" + this + "{reset}"
    member this.red =
        "{red}" + this + "{reset}"        