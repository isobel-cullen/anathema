module Anathema.Core.Systems.EntityBuilder

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Lenses

let player () =
    Entity.Default
        |> setAgency (Agency.Player)
        |> setPosition ({ Positionable.Actor with Symbol = '@' })