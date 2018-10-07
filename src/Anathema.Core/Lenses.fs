module Anathema.Core.Lenses

open Aether
open Aether.Operators

open Anathema.Core
open Anathema.Core.Components

[<AutoOpen>]
module private ComponentsLenses =
    let agencyLens =
        (fun (e: Entity) -> e.Agency),
        (fun a e -> { e with Agency = a |> Some })

    let positionLens =
        (fun (e: Entity) -> e.Positionable),
        (fun a e -> { e with Positionable = a |> Some })

    let destructableLens =
        (fun (e: Entity) -> e.Destructable),
        (fun a e -> { e with Destructable = a |> Some })

    let interactableLens =
        (fun (e: Entity) -> e.Interactable),
        (fun i e -> { e with Interactable = i |> Some })

[<AutoOpen>]
module private AgencyLenses =
    let energy_ =
        agencyLens >?> (
            (fun (a: Agency) -> a.Energy),
            (fun value a -> { a with Energy = value }))

    let speed_ =
        agencyLens >?> (
            (fun (a: Agency) -> a.Speed),
            (fun value a -> { a with Speed = value }))

module PositionLenses =
    let coords_ =
        positionLens >?> (
            (fun (p: Positionable) -> p.Coord),
            (fun value p -> { p with Coord = value }))

    let exclusive_ =
        positionLens >?> (
            (fun (p: Positionable) -> p.Exclusive),
            (fun value p -> { p with Exclusive = value }))

    let symbol_ =
        positionLens >?> (
            (fun (p: Positionable) -> p.Symbol),
            (fun value p -> { p with Symbol = value}))

module InteractableLenses =
    let mode_ =
        interactableLens >?> (
            (fun (i: Interactable) -> i.Mode),
            (fun value i -> { i with Mode = value })
        )
    let door_ =
            (fun m -> match m with
                      | Door (state,lockMode) -> Some (state, lockMode)
                      | _ -> None),
            (fun (ds) m -> match m with
                           | Door _ -> Door ds
                           | m -> m)

[<AutoOpen>]
module EntityGetters =
    let agency = Optic.get agencyLens
    let position = Optic.get positionLens
    let destructable = Optic.get destructableLens
    let interactable = Optic.get interactableLens

[<AutoOpen>]
module EntitySetters =
    let setAgency = Optic.set agencyLens
    let setPosition = Optic.set positionLens
    let setDestructable = Optic.set destructableLens
    let setInteractable = Optic.set interactableLens

module Agency =
    let energy = energy_ |> Optic.get
    let setEnergy = energy_ |> Optic.set
    let addEnergy =  energy_ |> Optic.map
    let speed = speed_ |> Optic.get

module Position =
    open PositionLenses

    let coords = coords_ |> Optic.get
    let setCoords = coords_ |> Optic.set

module Interaction =
    open InteractableLenses

    let mode = mode_ |> Optic.get
    let door = mode_ >?> door_ |> Optic.get
    let setDoor = mode_ >?> door_ |> Optic.set