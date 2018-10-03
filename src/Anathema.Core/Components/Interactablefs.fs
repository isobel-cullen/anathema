namespace Anathema.Core.Components

type LockMode =
| Unlocked
| Keycard
| Password of string
| ComputerControlled of int64

type DoorState = 
| Open 
| Closed 

type InteractionMode =
| Door of DoorState * LockMode

type Interactable = {
    Mode: InteractionMode
} with
    static member UnlockedDoor =
        { Mode = Door (Closed, Unlocked) }

