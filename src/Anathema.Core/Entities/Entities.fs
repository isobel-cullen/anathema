namespace Anathema.Core.Entities

open System.Collections.Immutable

open Anathema.Core.Components

[<NoComparison>]
type Entity = {
    Id: int64
    IsEnabled: bool
    Components: ImmutableArray<Component option>
} with 
    static member Create id =
        {
            Id = id; 
            IsEnabled = false; 
            Components = ImmutableArray.CreateRange (Array.zeroCreate (Component.``Max Number of Components``))
        }

    member this.Has<'t when 't :> Component> (index) =
        match this.Components.[index] with
        | Some _ -> true
        | None -> false

    member this.Get<'t when 't :> Component> (index) =
        match this.Components.[index] with
        | Some c -> c :?> 't |> Some
        | None -> None

    member this.With(index, comp) =
        { this with Components = this.Components.SetItem(index, comp |> Some) }

type EntitySystem = {
    Entities: Map<int64, Entity>
    NextId: int64
} with
    static member Empty =
        { Entities = Map.empty; NextId = 1L }
        
