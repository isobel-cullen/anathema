namespace Anathema.Core.Components
open Anathema.Core.Foundation

[<AbstractClass>]
type Component (name: string) =
    member this.Name = name
    with
        static member Agency                = 0
        static member Position              = 1
        static member Visibility            = 2

        // each entity gets an array of this capacity, each component
        // has a unique index. Should be fast to access with only uh,
        // 4 or 8 bytes "waste" per empty component but no resizing or
        // copying required. 
        static member ``Max Number of Components`` = 3

[<AbstractClass>]
type Agency () =
    inherit Component "Agency"
    member val Energy = 0 with get, set
    member val Speed = 10 with get, set
    
    abstract member RequiresInput: bool
    default this.RequiresInput = false

type AIControlled () =
    inherit Agency ()

type PlayerControlled () =
    inherit Agency ()
    override this.RequiresInput = true

type Position (point: Point) =
    inherit Component "Position"
    member this.Pos = point

type Visibility (symbol, ?asset) =
    inherit Component "Visibility"
    member this.Symbol: char = symbol
    member this.Asset = asset

