namespace Anathema.Core.Actions

open Anathema.Core.Foundation

type ActionResult = {
    IsComplete: bool
    IsSuccessful: bool
}

type ActionType =
| Idle
| Move of Direction

type Action = {
    EntityId: int64
    Cost: int
    Type: ActionType
} with
    static member Idle id =
        { EntityId = id; Cost = 10; Type = Idle }