namespace Anathema.Core.FrameworkExtensions

module Seq =
    let choose2 (p1: 't -> 'u option) (p2: 't -> 'w option) (source: 't seq) =
        source |> Seq.choose (fun item -> 
            match p1 item, p2 item with
            | None, _
            | _, None -> None
            | Some l, Some r -> Some (l, r))


module Map =
    let chooseValues chooser (map: Map<'k,'v>) =
        seq { for kv in map do
                match chooser (kv.Value) with
                | Some value -> yield value
                | None -> ()
            }