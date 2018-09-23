module Anathema.Core.Systems.Agency

open Anathema.Core
open Anathema.Core.Actions
open Anathema.Core.Components
open Anathema.Core.Entities
open Anathema.Core.Foundation

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