module Anathema.Core.Lenses

open Aether

open Anathema.Core.Components
open Anathema.Core.Entities

type Entity with
    static member LensFor<'t when 't :> Component> (index) =
        (fun (e: Entity) -> e.Get<'t>(index)),
        (fun (p: 't) (e: Entity) -> e.With(index, p)) 

[<AutoOpen>]
module private Components =
    let agency = Entity.LensFor<Agency>(Component.Agency)
    let position = Entity.LensFor<Position>(Component.Position)
    let visibility = Entity.LensFor<Visibility>(Component.Visibility)

module Get =
    let agency = Optic.get agency
    let position = Optic.get position
    let visibility = Optic.get visibility

module Set =
    let agency = Optic.set agency
    let position = Optic.set position
    let visibility = Optic.set visibility