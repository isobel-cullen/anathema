namespace Anathema.Core.Foundation

type Direction =
| Centre
| North
| NorthEast
| East
| SouthEast
| South
| SouthWest
| West
| NorthWest
    static member Random () =
        let rng = System.Random()
        match rng.Next(0,9) with
        | 0 -> Centre
        | 1 -> North
        | 2 -> NorthEast
        | 3 -> East
        | 4 -> SouthEast
        | 5 -> South
        | 6 -> SouthWest
        | 7 -> West
        | 8 -> NorthWest
        | _ -> failwith "impossible"

type Point = int * int
module Point =
    let zero = 0,0
    let one = 1,1

    let fromDirection = function
        | Centre        -> 0,0
        | North         -> 0,-1
        | NorthEast     -> 1,-1
        | East          -> 1,0
        | SouthEast     -> 1,1
        | South         -> 0,1
        | SouthWest     -> -1,1
        | West          -> -1,0
        | NorthWest     -> -1,-1

[<AutoOpen>]
module Operators =
    let inline (++) (x1: int ,y1: int) (x2,y2) = (x1 + x2), (y1 + y2)
    let inline (--) (x1: int, y1: int) (x2,y2) = (x1 - x2), (y1 - y2)