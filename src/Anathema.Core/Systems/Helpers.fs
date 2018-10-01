module Anathema.Core.Systems.Helpers

///
/// TODO: rename this or move anything it contains to other files since
/// "Helpers" is literally one of the worst names for things
///

open Anathema.Core
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses

let entityAt point (world: WorldState) =
    world.Entities
        |> Map.chooseItemsWith position
        |> Seq.tryFind (Position.coords >> Option.exists (fun p -> p = point))
