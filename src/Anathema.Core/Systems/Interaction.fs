module Anathema.Core.Systems.Interaction

open Aether
open Aether.Operators

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Lenses
open Anathema.Core.Lenses.Position
open Anathema.Core.Foundation
open Anathema.Core.Foundation.Point
open Anathema.Core.Systems.Helpers
open Anathema.Core.WorldState
open Anathema.Core.Lenses.InteractableLenses
open Anathema.Core.Lenses.PositionLenses

let chooseWith c e =
    match c e with
    | Some r -> (e, r) |> Some
    | None -> None

let interactableAt (world: WorldState) (p: Point) =
    world |> entityAt p |> Option.bind (chooseWith interactable)

let toggleDoorState =
    (mode_ >?> door_ |> Optic.map)
        (fun (door, l) ->
            match door with
            | Open -> Closed, l
            | Closed -> Open, l)
    >> (exclusive_ |> Optic.map)
        (fun e -> not e)
    >> (symbol_ |> Optic.map)
        (fun e -> if e = '+' then '/' else '+')

let perform (world: WorldState) (action: Action) =
    let entity = world.Entities.[action.EntityId]

    match action.Type, coords entity with
    | Interact dir, Some coords ->
        match interactableAt world (coords ++ Point.fromDirection dir) with
        | Some (iEntity, i) ->
            match i.Mode with
            | Door (_, Unlocked) -> world |> replace (toggleDoorState iEntity )
            | Door (doorState, lockMode) ->
                // TODO: unlocking and locking doors
                world
            | _ -> world
        | None -> world
    | _, _ -> world
