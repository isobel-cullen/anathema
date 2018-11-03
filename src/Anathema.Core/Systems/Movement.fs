module Anathema.Core.Systems.Movement

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Foundation
open Anathema.Core.Foundation.Point
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses
open Anathema.Core.Lenses.Components
open Anathema.Core.Lenses.Position

[<AutoOpen>]
module Impl =
    let isWithinBounds (world: WorldState) position (direction: Direction) =
       let xBound,yBound = world.Bounds
       match position.Coord ++ (Point.fromDirection direction) with
       | -1, _
       | _, -1 -> false
       | x, _ when x = xBound -> false
       | _, y when y = yBound -> false
       | _ -> true

    let isNotBlocked (state: WorldState) point (direction: Direction) =
        let prospectivePosision = point.Coord ++ (Point.fromDirection direction)
        state.EntitiesById
            |> Map.chooseValues position
            |> Seq.filter (exclusive >> Option.defaultValue false)
            |> Seq.exists (coords >> Option.exists ((=) prospectivePosision))
            |> not

let isMoveValid (state: WorldState) position (direction: Direction) =
    [
        isWithinBounds
        isNotBlocked
    ] |> List.forall (fun check -> check state position direction)

let perform (world: WorldState) (action: Action) =
    match action.Type with
    | Move dir -> 
        let entity = world.EntitiesById.[action.EntityId]
        match position entity with
        | None -> world
        | Some pos ->
            match isMoveValid world pos dir with
            | true ->
                let newPosition ={ pos with Coord = pos.Coord ++ Point.fromDirection dir }
                world.Replace (entity |> setPosition newPosition)
            | false -> world
    | _ -> world
