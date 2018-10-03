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
    let energyPrism =
        agencyLens >?> (
            (fun (a: Agency) -> a.Energy),
            (fun value a -> { a with Energy = value }))

    let speedPrism =
        agencyLens >?> (
            (fun (a: Agency) -> a.Speed),
            (fun value a -> { a with Speed = value }))

[<AutoOpen>]
module private PositionLenses =
    let coordsPrism =
        positionLens >?> (
            (fun (p: Positionable) -> p.Coord),
            (fun value p -> { p with Coord = value }))

module InteractableLenses =
    let modePrism =
        interactableLens >?> (
            (fun (i: Interactable) -> i.Mode),
            (fun value i -> { i with Mode = value })
        )
    let doorPrism =
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
    let energy = energyPrism |> Optic.get
    let setEnergy = energyPrism |> Optic.set
    let addEnergy =  energyPrism |> Optic.map
    let speed = speedPrism |> Optic.get

module Position=
    let coords = coordsPrism |> Optic.get
    let setCoords = coordsPrism |> Optic.set

module Interaction =
    open InteractableLenses
    
    let mode = modePrism |> Optic.get
    let door = modePrism >?> doorPrism |> Optic.get
    let setDoor = modePrism >?> doorPrism |> Optic.set