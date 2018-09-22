module Anathema.Core.Lenses

open Aether
open Anathema.Core.Entities

[<AutoOpen>]
module private Components =
    let agencyLens = (fun (e: Entity) -> e.Agency), (fun a e -> { e with Agency = a |> Some })
    let positionLens = (fun (e: Entity) -> e.Position), (fun a e -> { e with Position = a |> Some })
    let visibilityLens = (fun (e: Entity) -> e.Visible), (fun a e -> { e with Visible = a |> Some })

[<AutoOpen>]
module Getters = 
    let agency = Optic.get agencyLens
    let position = Optic.get positionLens
    let visibility = Optic.get visibilityLens

[<AutoOpen>]
module Setters = 
    let setAgency = Optic.set agencyLens
    let setPosition = Optic.set positionLens
    let setVisibility = Optic.set visibilityLens