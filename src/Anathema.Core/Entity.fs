namespace Anathema.Core

open Anathema.Core.Components

[<NoComparison>]
type Entity = {
    Id: int64
    IsEnabled: bool

    // Components
    Agency: Agency option
    Positionable: Positionable option
    Destructable: Destructable option
    Interactable: Interactable option
} with
    static member Default = {
        Id = 0L
        IsEnabled = false
        Agency = None
        Destructable = None
        Positionable = None
        Interactable = None
    }