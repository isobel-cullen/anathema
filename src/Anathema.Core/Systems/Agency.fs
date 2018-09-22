module Anathema.Core.Systems.Agency

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Entities
open Anathema.Core.Foundation

let canAct (agency: Agency option) =
    match agency with
    | Some agency when agency.Energy < 100 -> true 
    | _ -> false

let getAction (world: WorldState) =
    Action.Empty