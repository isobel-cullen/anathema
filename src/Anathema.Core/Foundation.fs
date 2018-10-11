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

module Lines =
    let bresenham (origin: Point) (destination: Point) =
        let x1,y1 = origin
        let x2,y2 = destination

        let dx = x2 - x1
        let dy = y2 - y1

        let mutable d = 2 * dy - dx
        let mutable y = y1

        seq {
            for x in x1 .. x2 do
                yield (x,y)
                if d > 0 then
                    y <- y + 1
                    d <- d - 2 * dx
                d <- d + 2 * dy
            }

    module Rect =
        let fromOppositeCorners (c1: Point) (c2: Point) =
            let x1,y1 = c1
            let x2,y2 = c2

            seq {
                for x in x1 .. x2 do
                    for y in y1 .. y2 do
                        yield (x,y)
            }

        let fromDimensions origin x y =
            if x <= 1 && y <= 0 then
                Seq.empty
            else
                let ox,oy = origin
                fromOppositeCorners origin (ox + (x-1), oy + (y-1))

    module Circle =
        let bresenham (origin: Point) (radius: int) =
            let allOctants (ox,oy) x y =
                [
                    ox + x, oy + y
                    ox - x, oy + y
                    ox + x, oy - y
                    ox - x, oy - y
                    ox + y, oy + x
                    ox - y, oy + x
                    ox + y, oy - x
                    ox - y, oy - x
                ]

            match radius with
            | 1 -> [ origin ]
            | 2 ->
                [
                    origin
                    origin ++ Point.fromDirection North
                    origin ++ Point.fromDirection East
                    origin ++ Point.fromDirection West
                    origin ++ Point.fromDirection South
                ]
            | _ ->
                // from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
                // pretty sure it is broken
                let mutable x = 0
                let mutable y = radius
                let mutable d = radius

                [
                    while y >= x do
                        yield! allOctants origin x y
                        x <- x + 1

                        if d > 0 then
                            y <- y - 1
                            d <- d + 4 * (x - y) + 10
                        else
                            d <- d + 4 * 6

                        yield! allOctants origin x y
                ]
