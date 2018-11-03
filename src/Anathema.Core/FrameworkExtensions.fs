namespace Anathema.Core.FrameworkExtensions

module Seq =
    let choose2 (p1: 't -> 'u option) (p2: 't -> 'w option) (source: 't seq) =
        source |> Seq.choose (fun item -> 
            match p1 item, p2 item with
            | None, _
            | _, None -> None
            | Some l, Some r -> Some (l, r))


module Map =
    /// return values where the chooser function returns Some 'a
    let chooseValues chooser (map: Map<'k,'v>) =
        seq { for kv in map do
                match chooser (kv.Value) with
                | Some value -> yield kv.Value
                | None -> ()
            }


module Option =
    let withDefault (def: 'b) (binder: 'a -> 'b option) =
        fun (x: 'a) ->
            match binder x with
            | Some value -> value
            | None -> def
            
    let lift (binder: 'a -> 'b) (optionf: 'c -> 'a option) =
        fun x -> Option.map binder (optionf x)


module Operators =
    module Options =
        let (>>?) f d = Option.withDefault d f
        let (>>=) opt binder = Option.lift binder opt