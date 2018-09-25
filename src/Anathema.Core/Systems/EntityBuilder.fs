module Anathema.Core.Systems.EntityBuilder

open Anathema.Core.Components
open Anathema.Core.Entities
open Anathema.Core.Lenses

let player () =
    Entity.Default
        |> setAgency (Agency.Player)
        |> setVisibility ({ Visible.Actor with Symbol = '@' })