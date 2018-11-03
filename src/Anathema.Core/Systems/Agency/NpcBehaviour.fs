module Anathema.Core.Systems.NpcBehaviour

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses
open Anathema.Core.Lenses.Agency
open Anathema.Core.Lenses.Components
open Anathema.Core.Lenses.Position
open Anathema.Core.Lenses.PositionLenses
open Anathema.Core.Systems.Pathfinding
open Anathema.Core.Foundation

open Aether
open Aether.Operators

let bigIfTrue = function
    | Some true -> Some true
    | _ -> None

let blockedCells (world: WorldState) =
    world.EntitiesById
        |> Map.chooseValues (exclusive >> bigIfTrue)
        |> Seq.choose coords

let goals (world: WorldState) = 
    world.EntitiesById
        |> Map.toSeq
        |> Seq.filter (snd >> isPlayer)
        |> Seq.choose (snd >> coords)

let pointsAround xBounds yBounds coords =
    let isValid (x,y) =
        (x >= 0 && x < xBounds)
            && (y >= 0 && y < yBounds)
    Point.around coords |> List.filter isValid

/// Work out what the CurrentEntity is going to do next
let getNextAction (world: WorldState) behaviour =
    let npc = world.CurrentEntity
    match behaviour with
    | Aggressive -> 
        let blocked = blockedCells world
        let goals = goals world

        if System.Diagnostics.Debugger.IsAttached then
            let interacting = world.EntitiesById |> Map.chooseValues interactable |> Seq.toArray
            interacting |> ignore

        let xBound,yBound = world.Bounds

        let path = generateDijkstraMap xBound yBound blocked goals

        let npcPos = npc.Positionable.Value.Coord
        let nx,ny = npcPos

        let gx,gy = 
            pointsAround xBound yBound npcPos
                |> List.minBy (fun (x,y) -> path.[x,y])
                         
        let direction = Direction.FromXY ( gx-nx, gy-ny )

        npc |> Action.move direction |> Some
    | Wandering -> 
        {
            EntityId = world.CurrentEntity.Id
            Type = Move <| Direction.Random()
            Cost = 50
        } |> Some
    | Passive ->
        Action.Idle (npc.Id) |> Some

        