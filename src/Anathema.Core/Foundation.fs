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
    member this.ToPoint () =
            match this with
            | North         -> { X = 0; Y = -1 }
            | NorthEast     -> { X = 1; Y = -1 }
            | East          -> { X = 1; Y = 0 }
            | SouthEast     -> { X = 1; Y = 1 }
            | South         -> { X = 0; Y = 1 }
            | SouthWest     -> { X = -1; Y = 1 }
            | West          -> { X = -1; Y = 0 }
            | NorthWest     -> { X = -1; Y = -1 }

    static member Random () =
        let rng = System.Random()
        match rng.Next(0,8) with
        | 0 -> North
        | 1 -> NorthEast
        | 2 -> East
        | 3 -> SouthEast
        | 4 -> South
        | 5 -> SouthWest
        | 6 -> West
        | 7 -> NorthWest
        | _ -> failwith "impossible"
