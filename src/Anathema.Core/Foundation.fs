namespace Anathema.Core.Foundation

type Point = { X: int; Y: int } with 
    static member Zero = { X=0; Y=0 }
    static member One = { X=1; Y=1 }
    static member (+) (l,r) = { X = l.X + r.X; Y = l.Y + r.Y }
    static member (-) (l,r) = { X = l.X - r.X; Y = l.Y - r.Y }

type Direction =
| North
| NorthEast
| East
| SouthEast
| South
| SouthWest
| West
| NorthWest
    with member this.ToPoint () =
            match this with
            | North         -> { X = 0; Y = -1 }
            | NorthEast     -> { X = 1; Y = -1 } 
            | East          -> { X = 1; Y = 0 }
            | SouthEast     -> { X = 1; Y = 1 }
            | South         -> { X = 0; Y = 1 }
            | SouthWest     -> { X = -1; Y = 1 }
            | West          -> { X = -1; Y = 0 }
            | NorthWest     -> { X = -1; Y = -1 }

type ActionType =
| Idle
| Move of Direction
    
type Action = {
    EntityId: int64
    Cost: int
    Type: ActionType
} with 
    static member Empty id =
        { EntityId = id; Cost = 10; Type = Idle }