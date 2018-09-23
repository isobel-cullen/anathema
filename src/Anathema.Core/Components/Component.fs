namespace Anathema.Core.Components
open Anathema.Core.Foundation

type BehaviourResolver =
| Passive
| Wandering
| Aggressive

type Agency = {
    Energy: int
    Speed: int
    RequiresInput: bool
    Behaviour: BehaviourResolver option
} with 
    static member Default = { 
        Energy = 0
        Speed = 0
        RequiresInput = false
        Behaviour = None
    }

    static member Player = { Agency.Default with RequiresInput = true; Speed = 10 }
    static member Wandering = { Agency.Default with Behaviour = (Some Wandering); Speed = 10 }

type Visible = {
    Symbol: char
}

