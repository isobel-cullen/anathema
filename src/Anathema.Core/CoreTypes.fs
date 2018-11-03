namespace Anathema.Core

open System.Collections.Immutable

open Anathema.Core.Components
open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions

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

type ActionResult = {
    IsComplete: bool
    IsSuccessful: bool
}

type ActionType =
| Idle
| Move of Direction
| Interact of Direction
| MeleeAttack of Direction // TODO: ranged attacks

type Action = {
    EntityId: int64
    Cost: int
    Type: ActionType
} with
    static member Idle id =
        { EntityId = id; Cost = 10; Type = Idle }

module Action =
    let move direction (entity: Entity) =
        {
            EntityId = entity.Id
            Cost = 50
            Type = Move direction
        }

type WorldState = {
    ActionQueue: ImmutableQueue<Action>
    EntitiesById: Map<int64, Entity>
    NextId: int64
    EntityCounter: int
    CounterToEntity: ImmutableArray<int64>
    Bounds: int * int
} with
    static member Empty =
        {
            ActionQueue = ImmutableQueue.Empty
            EntitiesById = Map.empty
            NextId = 1L
            EntityCounter = 0
            CounterToEntity = ImmutableArray.Empty
            Bounds = 60, 20
        }

    member __.Register entity =
        let entityWithId = { entity with Id = __.NextId; IsEnabled = true }
        { __ with
            CounterToEntity = __.CounterToEntity.Add (__.NextId) 
            EntitiesById = __.EntitiesById.Add (__.NextId, entityWithId)
            NextId = __.NextId + 1L
        }

    member __.Replace (entity: Entity) =
        match entity.Id < __.NextId with
        | false -> failwith "Entities need to be registered first"
        | true -> { __ with EntitiesById = __.EntitiesById.Add (entity.Id, entity) }

    member __.Remove (entity: Entity) =
        { __ with 
            EntitiesById = __.EntitiesById.Remove (entity.Id) 
            CounterToEntity = __.CounterToEntity.Remove (entity.Id)     
        }

    member __.CurrentEntity =
        let id = __.CounterToEntity.[__.EntityCounter]
        __.EntitiesById.[id]

    member __.AdvanceEntityCounter () =
        let nextCounter = __.EntityCounter + 1
        match nextCounter, __.EntitiesById.Count with
        | c,ec when c >= ec -> { __ with EntityCounter = 0 }
        | c, _ -> { __ with EntityCounter = nextCounter }

module WorldState =
    let replace entity (state: WorldState) = state.Replace entity
    let register entity (state: WorldState) = state.Register entity
