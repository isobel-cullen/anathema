namespace Anathema.Core.Entities

open System.Collections.Immutable

open Anathema.Core.Components
open Anathema.Core.Foundation

[<NoComparison>]
type Entity = {
    Id: int64
    IsEnabled: bool

    // Components
    Agency: Agency option
    Visible: Visible option
    Position: Position option
} with 
    static member Default = {
        Id = 0L
        IsEnabled = false
        Agency = None
        Visible = None
        Position = None
    }