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

    static member Player =
        { Agency.Default with RequiresInput = true; Speed = 10 }

    static member Wandering =
        { Agency.Default with Behaviour = (Some Wandering); Speed = 10 }

type Layer =
| Background    // what is the ground is made of, eq water | dirt
| OnFloor         // what is on the ground, eq items
| Foreground    // what is the "main" thing there, eq @ or a door

type Visible = {
    Symbol: char
    Layer: Layer
} with 
    static member Floor = { Symbol = '.'; Layer = Background }
    static member Doodad = { Symbol = '*'; Layer = OnFloor }
    static member Actor = { Symbol = '?'; Layer = Foreground }

type Position = {
    Coord: Point
    Exclusive: bool
} with 
    static member Default = { Coord = Point.Zero; Exclusive = false }
    static member Actor = { Position.Default with Exclusive = true }