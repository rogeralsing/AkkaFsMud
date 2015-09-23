module Messages
open Akka.Actor

type NamedObject = {name: string; ref: IActorRef;}

type Message = 
| Message of format: string * args: list<obj>

type ThingMessage =
| SetOutput of IActorRef
| SetContainer of IActorRef
| ContainerLook of who : NamedObject
| ContainerRemove of NamedObject
| ContainerAdd of NamedObject
| GetName
| Where
| Inventory
| Look
| Say of message: string
| Notify of Message
| ContainerNotify of Message * except: seq<IActorRef>
| FindByName of nameToFind: string * except: seq<IActorRef>
| FindObjectByName of string 
| FoundObjectByName of Option<(IActorRef * string)>
| Take of nameOfObject: string
| Drop of nameOfObject: string
| Put of nameOfObjectToTake: string * nameOfContainer: string
| Fight of nameOfTarget: string
| SetTarget of IActorRef
| AttackCurrentTarget
| TakeDamage of attacker: IActorRef * damage: int
| NotifyCombatStatus
| Died
