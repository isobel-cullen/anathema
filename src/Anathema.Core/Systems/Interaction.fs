module Anathema.Core.Systems.Interaction

open Anathema.Core

let perform world (action: Action) =
    match action.Type with
    | Interact dir ->

        world
    | _ -> world
