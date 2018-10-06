module Anathema.Core.Systems.Agency

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses
open Anathema.Core.Systems.Helpers

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
| Interaction of Direction
| ForceAttack of Direction
| PlayerAction of Action

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

let shouldInteract interactable =
    match interactable.Mode with
    | Door (Closed, _) -> true
    | _ -> false

let hasAgency e =
    match agency e with
    | Some _ -> true
    | None -> false

let playerMoveToAction (world: WorldState) (id: int64) (command: PlayerCommand) =
    // TODO: implement this when there are actually non-hostile
    // agents
    let areHostile subject target =
        true

    let entity = world.Entities.[id]
    match entity.Agency with
    | Some a when a.RequiresInput ->
        match command, (position entity) with
        | Dir direction, Some pos ->
            let pointOfInterest = (pos.Coord) ++ Point.fromDirection direction
            match world |> entityAt pointOfInterest with
            | None -> Action.move direction entity
            | Some target when hasAgency target ->
                match areHostile entity target with
                | false -> Action.Idle (entity.Id)
                | true ->
                    {
                        EntityId = entity.Id
                        Cost = 75
                        Type = MeleeAttack direction
                    }
            | Some other ->
                match interactable other with
                | Some i when shouldInteract i ->
                    {
                        EntityId = entity.Id
                        Cost = 50
                        Type = Interact direction
                    }
                | _ -> entity |> Action.move direction
        | PlayerAction act, _ -> act
        | _ -> Action.Idle (entity.Id)
    | _ -> failwith "Shouldn't get here?"

let dispatchAction world (action: Action) =
    let exhaust ent =
        match ent |> agency with
        | Some ag -> ent |> Agency.setEnergy (ag.Energy - action.Cost)
        | None -> ent

    match action.Type with
    | Idle -> world
    | Move dir -> Movement.perform world action
    | Interact dir -> Interaction.perform world action
    | _ -> world
