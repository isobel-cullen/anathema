module Anathema.Core.Systems.Movement

open Anathema.Core
open Anathema.Core.Foundation
open Anathema.Core.Components
open Anathema.Core.Lenses

let perform (world: WorldState) (action: Action) =
    match action.Type with
    | Move dir -> 
        let entity = world.Entities.[action.EntityId]
        match Get.position entity with
        | None -> world
        | Some pos ->
            world.Replace 
                (entity |> Set.position (Position (pos.Pos + dir.ToPoint())))
    | _ -> world
