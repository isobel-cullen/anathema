module Anathema.Core.Systems.Pathfinding

open System
// dijkstra maps as described here http://www.roguebasin.com/index.php?title=The_Incredible_Power_of_Dijkstra_Maps

let inline private neighbours x y arr =
    let boundsX = (Array2D.length1 arr) - 1
    let boundsY = (Array2D.length2 arr) - 1

    match x, y with
    | 0,0 -> [|struct(1,0); struct(0,1)|]
    | x,y when x = boundsX && y = boundsY -> [|struct(x-1,y); struct(x,y-1)|]
    | 0, y when y = boundsY -> [|struct(1,y); struct(0,y-1)|]
    | x,0 when x = boundsX -> [|struct(x-1,0);struct(x,1)|]
    | 0, y -> [|struct(0,y-1); struct(0,y+1); struct(1,y)|]
    | x,0 -> [|struct(x,1); struct(x+1,0); struct(x-1,0)|]
    | x,y when y = boundsY -> [|struct(x,y-1); struct(x+1,y); struct(x-1,y)|]
    | x,y when x = boundsX -> [|struct(x,y-1); struct(x,y+1); struct(x-1,y)|]
    | x,y -> [|struct(x,y-1); struct(x,y+1); struct(x+1,y); struct(x-1,y)|]

let generateDijkstraMap x y blocked goals =
    let ceiling = (x * y)
    let arena = Array2D.create x y ceiling 

    for (bx,by) in blocked do
        arena.[bx,by] <- Int32.MaxValue

    for (gx,gy) in goals do
        arena.[gx,gy] <- 0
        
    let initialSet = goals 
                    |> Seq.collect (fun (x,y) -> neighbours x y arena)
                    |> Seq.filter (fun struct(x,y) -> arena.[x,y] = ceiling)
                    |> Seq.toArray

    let inline isCeiling struct(x,y) = arena.[x,y] = ceiling
    
    let inline nextItems level struct(x,y) =
        if arena.[x,y] = ceiling then
            arena.[x,y] <- level
            neighbours x y arena
        else Array.empty

    let rec fanOut level (items: struct (int * int) array) =
        if items.Length > 0 then
            fanOut (level + 1) 
                (items |> Array.collect (nextItems level)
                    |> Array.filter (isCeiling))
                    
    fanOut 0 initialSet
    arena