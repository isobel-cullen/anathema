module Anathema.Core.Lenses

open Aether
open Aether.Operators

open Anathema.Core
open Anathema.Core.Components

module private Int =
    let id_ = fun f _ x -> f x


module Components =
    let agency_ =
        (fun (e: Entity) -> e.Agency),
        (fun a e -> { e with Agency = a |> Some })

    let position_ =
        (fun (e: Entity) -> e.Positionable),
        (fun a e -> { e with Positionable = a |> Some })

    let destructableLens =
        (fun (e: Entity) -> e.Destructable),
        (fun a e -> { e with Destructable = a |> Some })

    let interactableLens =
        (fun (e: Entity) -> e.Interactable),
        (fun i e -> { e with Interactable = i |> Some })

module AgencyLenses =
    let energy_ =
            (fun (a: Agency) -> a.Energy),
            (fun value a -> { a with Energy = value })

    let kind_ =
            (fun (a: Agency) -> a.Kind),
            (fun value a -> { a with Kind = value})

    let stats_ =
            (fun (a: Agency) -> a.Stats),
            (fun value a -> { a with Stats = value })

module PositionLenses =
    let coords_ =
            (fun (p: Positionable) -> p.Coord),
            (fun value p -> { p with Coord = value })

    let exclusive_ =
            (fun (p: Positionable) -> p.Exclusive),
            (fun value p -> { p with Exclusive = value })

    let symbol_ =
            (fun (p: Positionable) -> p.Symbol),
            (fun value p -> { p with Symbol = value})

module InteractableLenses =
    let mode_ =
            (fun (i: Interactable) -> i.Mode),
            (fun value i -> { i with Mode = value })

    let door_ =
            (fun m -> match m with
                      | Door (state,lockMode) -> Some (state, lockMode)
                      | _ -> None),
            (fun (ds) m -> match m with
                           | Door _ -> Door ds
                           | m -> m)

module CharacteristicLenses =
    let agility_ =
        (fun (c: Characteristics) -> c.Agility),
        (fun value c -> { c with Agility = value })

[<AutoOpen>]
module EntityGetters =
    open Components

    let agency = Optic.get agency_
    let position = Optic.get position_
    let destructable = Optic.get destructableLens
    let interactable = Optic.get interactableLens

[<AutoOpen>]
module EntitySetters =
    open Components

    let setAgency = Optic.set agency_
    let setPosition = Optic.set position_
    let setDestructable = Optic.set destructableLens
    let setInteractable = Optic.set interactableLens

module Agency =
    open AgencyLenses

    let energy = energy_ |> Optic.get
    let addEnergy = Components.agency_ >?> energy_ |> Optic.map

module Characteristics =
    open CharacteristicLenses
    let private ag = (Components.agency_) >?> (AgencyLenses.stats_) >?> agility_

    let agility = ag |> Optic.get
    let setAgilty = ag |> Optic.set



module Position =
    open PositionLenses

    let coords = Components.position_ >?> coords_ |> Optic.get
    let setCoords = Components.position_ >?> coords_ |> Optic.set

module Interaction =
    open InteractableLenses

    let mode = mode_ |> Optic.get
    let door = mode_ >-> door_ |> Optic.get
    let setDoor = mode_ >-> door_ |> Optic.set