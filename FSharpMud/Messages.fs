module Messages
open Akka.Actor

type NamedObject = {name: string; ref: IActorRef;}

type Message = 
| Message of format: string * args: list<obj>

type ThingMessage =
| SetOutput of IActorRef
| SetContainer of IActorRef
| ContainerLook of who : NamedObject
| ContainerAdd of NamedObject
| ContainerAdded of NamedObject
| ContainerRemove of NamedObject
| ContainerRemoved of NamedObject
| ContainerContent of List<NamedObject>
| Where
| Inventory
| Look
| Say of message: string
| Notify of Message
| ContainerNotify of Message * except: seq<IActorRef>
| Take of nameOfObject: string
| Drop of nameOfObject: string
| Put of nameOfObjectToTake: string * nameOfContainer: string
| Fight of nameOfTarget: string
| SetTarget of NamedObject
| AttackCurrentTarget
| TakeDamage of attacker: NamedObject * damage: int
| NotifyCombatStatus
| Died