namespace Anathema.Core.Components

type Layer =
| Background    // what is the ground is made of, eq water | dirt
| OnFloor         // what is on the ground, eq items
| Foreground    // what is the "main" thing there, eq @ or a door

type Positionable = {
    Exclusive: bool
    Visible: bool
    Coord: (int * int)
    Symbol: char
    Layer: Layer
} with
    static member Default = {
        Coord = (0,0)
        Exclusive = true
        Symbol = '?'
        Visible = true
        Layer = Foreground
    }

    static member Floor =
        { Positionable.Default with Symbol = '.'; Layer = Background }
        
    static member Doodad =
        { Positionable.Default with Symbol = '*'; Layer = OnFloor }

    static member Actor =
        { Positionable.Default with Symbol = '?'; Layer = Foreground }