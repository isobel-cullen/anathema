module FoundationTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit
open FsUnit.Xunit

open Anathema.Core.Foundation.Lines

[<Fact>]
let ``Line points should be in order asc`` () =
    bresenham (0,0) (10,0) |> should be ascending

[<Fact>]
let ``Line points should be in order desc`` () =
    bresenham (0,10) (0,0) |> should be descending

[<Fact>]
let ``Rect should give 4 points for 0,0 to 1,1`` () =
    Rect.fromDimensions (0,0) 1 1
        |> List.ofSeq
        |> should haveLength 4

[<Property>]
let ``Rect should give unique points`` (o:int*int, l:int, h:int) =
    (Rect.fromDimensions o l h) |> should be unique

