module Messages
open Akka.Actor
open ActorState

type Message = 
    | Message of format : string * args : list<obj>

type ContainerMessages = 
    | AddContent of what:NamedObject * who:NamedObject
    | RemoveContent of who : NamedObject * newContaner : NamedObject
    | EnterRoom of who : NamedObject * from : NamedObject
    | ExitContainer of who : NamedObject * from : NamedObject
    | AddExit of NamedObject
    | ContainerNotify of Message * except : list<IActorRef>

type ContainedMessages = 
    | SetContainerByActorRef of IActorRef
    | NewContainerAssigned of container : NamedObject * content : Set<NamedObject> * exits : Set<NamedObject>
    | AddedContent of NamedObject
    | RemovedContent of who : NamedObject * newContaner : NamedObject

type NotifyMessages = 
    | SetOutput of IActorRef
    | Notify of Message

type AgentMessages = 
    | Inventory
    | Look
    | Go of direction : string
    | Say of message : string
    | Yell of message : string
    | Take of nameOfObject : string
    | Enter of nameOfObject : string
    | Exit
    | Drop of nameOfObject : string
    | Put of nameOfObjectToTake : string * nameOfContainer : string

type CombatMessages = 
    | Fight of nameOfTarget : string
    | SetTarget of NamedObject
    | AttackCurrentTarget
    | TakeDamage of attacker : NamedObject * damage : int
    | NotifyCombatStatus
    | Died