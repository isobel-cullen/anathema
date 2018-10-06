namespace Anathema.Core

open Anathema.Core.Components
open Anathema.Core.Lenses

type World (resumeFromState) =
    let mutable currentState = resumeFromState
    let mutable nextPlayerAction = None

    let advance (state: WorldState) = state.AdvanceEntityCounter()

    let exhaust agency action entity =
        entity |> setAgency
            { agency with Energy = agency.Energy - action.Cost }

    let energise (agency: Agency) = { agency with Energy = agency.Energy + agency.Speed }

    let canAct (agency: Agency) = agency.Energy >= 100
    let requiresInput (agency: Agency) = agency.RequiresInput

    let rec doActions (state: WorldState) =
        match state.ActionQueue.IsEmpty with
        | true -> state
        | false ->
            let action = state.ActionQueue.Peek()
            let state =
                { (Systems.Agency.dispatchAction state action) with
                    ActionQueue = state.ActionQueue.Dequeue() }
            doActions state 

    let rec getActions (state: WorldState) =
        let entity = state.CurrentEntity
        match agency entity with
        | Some agency ->
            match agency |> canAct, agency.RequiresInput, nextPlayerAction with
            | false, _, _ ->
                entity 
                    |> setAgency (energise agency)
                    |> state.Replace 
                    |> advance
                    |> getActions
            | true, false, _ ->
                match Systems.Agency.getAction state agency with
                | Some action -> 
                    let e = entity |> exhaust agency action
                    { state with 
                        ActionQueue = state.ActionQueue.Enqueue action 
                        Entities = state.Entities.Add (e.Id, e)
                    } |> advance |> getActions
                | _ -> state |> advance
            | true, true, None -> state
            | true, true, Some pA ->
                nextPlayerAction <- None
                let action = Systems.Agency.playerMoveToAction (currentState) (entity.Id) pA
                let actionWithId = { action with EntityId = entity.Id }
                let e = entity |> exhaust agency actionWithId
                { state with
                    ActionQueue = state.ActionQueue.Enqueue actionWithId
                    Entities = state.Entities.Add (e.Id, e)
                }
        | _ ->
            state |> advance |> getActions

    let run = getActions >> doActions

    new () = World WorldState.Empty

    member __.SetNextAction (action: Systems.Agency.PlayerCommand) =
        if nextPlayerAction.IsNone then nextPlayerAction <- action |> Some

    member __.Process () =
        currentState <- run currentState
        currentState
