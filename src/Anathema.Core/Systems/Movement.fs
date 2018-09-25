module Anathema.Core.Systems.Movement

open Anathema.Core
open Anathema.Core.Actions
open Anathema.Core.Entities
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Foundation
open Anathema.Core.Components
open Anathema.Core.Lenses

[<AutoOpen>]
module Impl =
    // consider calculating the new point prior to calling each checking function?
    let isWithinBounds (state: WorldState) (position: Position) (direction: Direction) =
       match position.Coord + (direction.ToPoint()) with
       | { X = -1 }
       | { X = 80 }
       | { Y = -1 }
       | { Y = 25 } -> false
       | _ -> true

    let isNotBlocked (state: WorldState) (point: Position) (direction: Direction) =
        let prospectivePosision = point.Coord + (direction.ToPoint())
        state.Entities
            |> Map.chooseValues position
            |> Seq.filter (fun p -> p.Exclusive)
            |> Seq.exists (fun p -> p.Coord = prospectivePosision)
            |> not

let isMoveValid (state: WorldState) (position: Position) (direction: Direction) =
    [
        isWithinBounds
        isNotBlocked
    ] |> List.forall (fun check -> check state position direction)

let perform (world: WorldState) (action: Action) =
    match action.Type with
    | Move dir -> 
        let entity = world.Entities.[action.EntityId]
        match position entity with
        | None -> world
        | Some pos ->
            match isMoveValid world pos dir with
            | true ->
                let newPosition ={ pos with Coord = pos.Coord + dir.ToPoint() }
                world.Replace (entity |> setPosition newPosition)
            | false -> world
    | _ -> world
