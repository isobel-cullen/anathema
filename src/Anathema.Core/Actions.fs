namespace Anathema.Core.Actions

open Anathema.Core
open Anathema.Core.Foundation

type ActionResult = {
    IsComplete: bool
    IsSuccessful: bool
}

type ActionType =
| Idle
| Move of Direction
| Interact of Direction
| Attack of Direction // TODO: ranged attacks


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