module Anathema.Core.Systems.Helpers

///
/// TODO: rename this or move anything it contains to other files since
/// "Helpers" is literally one of the worst names for things
///

open Anathema.Core
open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Lenses

let atPosition point =
    Position.coords >> Option.exists (fun p -> p = point)

let entityAt (point: Point) (world: WorldState) =
    world.Entities
        |> Seq.tryFind (fun kv -> kv.Value |> atPosition point)
        |> Option.map (fun kv -> kv.Value)