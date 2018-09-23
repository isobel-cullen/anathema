namespace Anathema.Core

open System.Collections.Immutable

open Anathema.Core.Actions
open Anathema.Core.Entities
open Anathema.Core.Foundation

type WorldState = {
    ActionQueue: ImmutableQueue<Action>
    Entities: Map<int64, Entity>
    NextId: int64
    EntityCounter: int
    CounterToEntity: ImmutableArray<int64>
} with
    static member Empty =
        { 
            ActionQueue = ImmutableQueue.Empty
            Entities = Map.empty
            NextId = 1L
            EntityCounter = 0
            CounterToEntity = ImmutableArray.Empty
        }

    member __.Register entity =
        let entityWithId = { entity with Id = __.NextId; IsEnabled = true }
        { __ with
            CounterToEntity = __.CounterToEntity.Add (__.NextId) 
            Entities = __.Entities.Add (__.NextId, entityWithId)
            NextId = __.NextId + 1L
        }

    member __.Replace (entity: Entity) =
        match entity.Id < __.NextId with
        | false -> failwith "Entities need to be registered first"
        | true -> { __ with Entities = __.Entities.Add (entity.Id, entity) }

    member __.Remove (entity: Entity) =
        { __ with 
            Entities = __.Entities.Remove (entity.Id) 
            CounterToEntity = __.CounterToEntity.Remove (entity.Id)     
        }

    member __.CurrentEntity =
        let id = __.CounterToEntity.[__.EntityCounter]
        __.Entities.[id]

    member __.AdvanceEntityCounter () =
        let nextCounter = __.EntityCounter + 1
        match nextCounter, __.Entities.Count with
        | c,ec when c >= ec -> { __ with EntityCounter = 0 }
        | c, _ -> { __ with EntityCounter = nextCounter }