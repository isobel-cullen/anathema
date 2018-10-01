module Anathema.Core.Systems.Movement

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses

[<AutoOpen>]
module Impl =
    /// TODO: This will need fixed when the arena becomes larger than the
    /// visible area.
    let isWithinBounds _ position (direction: Direction) =
       match position.Coord ++ (Point.fromDirection direction) with
       | -1, _
       | 80, _
       | _, -1
       | _, 25 -> false
       | _ -> true

    let isNotBlocked (state: WorldState) point (direction: Direction) =
        let prospectivePosision = point.Coord ++ (Point.fromDirection direction)
        state.Entities
            |> Map.chooseValues position
            |> Seq.filter (fun p -> p.Exclusive)
            |> Seq.exists (fun p -> p.Coord = prospectivePosision)
            |> not

let isMoveValid (state: WorldState) position (direction: Direction) =
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
                let newPosition ={ pos with Coord = pos.Coord ++ Point.fromDirection dir }
                world.Replace (entity |> setPosition newPosition)
            | false -> world
    | _ -> world
