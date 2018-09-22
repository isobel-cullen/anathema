namespace Anathema.Core

open System.Collections.Generic
open System.Collections.Immutable

open Anathema.Core.Foundation
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Entities
open Anathema.Core.Lenses

type World (resumeFromState) = 
    let mutable currentState = resumeFromState
    let mutable nextPlayerAction = None

    let rec doActions (state: WorldState) =
        match state.ActionQueue.IsEmpty with
        | true -> state
        | false ->
            let action = state.ActionQueue.Peek()
            let state = 
                { (Systems.Movement.perform state action) with
                    ActionQueue = state.ActionQueue.Dequeue() }
            doActions state 

    let rec getActions (state: WorldState) =
        let entity = state.CurrentEntity
        match Get.agency entity with
        | Some agency -> 
            match agency.Energy >= 100 with
            | true ->
                match agency.RequiresInput, nextPlayerAction with
                | false, _ ->
                    // TODO: feed entity in some AI system to get
                    // an action, add action to state, exit
                    state
                | true, Some action -> 
                    nextPlayerAction <- None
                    let actionWithId = { action with EntityId = entity.Id }
                    { state with ActionQueue = state.ActionQueue.Enqueue actionWithId }
                | _ -> state
            | _ ->
                // TODO: figure out how to make components immutable
                agency.Energy <- agency.Energy + agency.Speed
                state 
        | _ -> getActions (state.AdvanceEntityCounter ())

    let run = getActions >> doActions

    new () = World WorldState.Empty

    member __.SetNextAction (action: Foundation.Action) =
        if nextPlayerAction.IsNone then nextPlayerAction <- action |> Some

    member __.Process () =
        currentState <- run currentState
        currentState
        