module Anathema.Core.Systems.Agency

open Anathema.Core
open Anathema.Core.Actions
open Anathema.Core.Components
open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses

/// This idea of this time is that the UI shouldn't have to concern itself with
/// the details of actions, in normal 'bump' style combat the task of
/// deciding if a direction key should correspond to a move action or an attack
/// should be handled in the engine.
///
/// There will be some exceptions later, like shift-direction being
/// "force attack".
///
/// What I'd also like to handle is Qud-style "smart interaction", so standing
/// next to something like a door and mashing the spacebar will interact with it
/// without players having to specify the direction they want to interact in.
///
/// Of course in situations where the auto-action cannot be automatically
/// determined (due to more than one candidate action being possible) then they
/// will have to specify a direction anyway.
type PlayerCommand =
| Dir of Direction
| Interact of Direction
| ForceAttack of Direction

let getAction (world: WorldState) (agency: Agency) =
    match agency with
    | { Behaviour=Some behaviour; RequiresInput=false } ->
        match behaviour with
        | _ ->
            {
                EntityId = world.CurrentEntity.Id
                Type = Move <| Direction.Random()
                Cost = 50
            } |> Some
    | _-> None




let entityAt point (world: WorldState) =
    world.Entities
        |> Map.chooseItemsWith position
        |> Seq.tryFind (Position.coords >> Option.exists (fun p -> p = point))

let playerMoveToAction (world: WorldState) (id: int64) (command: PlayerCommand) =
    let entity = world.Entities.[id]
    match entity.Agency with
    | Some a when a.RequiresInput ->
        match command, (position entity) with
        | Dir direction, Some pos ->
            let pointOfInterest = (pos.Coord) ++ Point.fromDirection direction
            match world |> entityAt pointOfInterest with
            | _ -> Action.move direction entity
        | _ -> Action.Idle (entity.Id)
    | _ -> failwith "Shouldn't get here?"