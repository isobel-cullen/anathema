module FoundationTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit
open FsUnit.Xunit

open Anathema.Core.Foundation.Lines
open Anathema.Core.Foundation

let uncurry f (a,b) = f a b

[<Fact>]
let ``Line points should be in order asc`` () =
    bresenham (0,0) (10,0) |> should be ascending

[<Fact>]
let ``Line points should be in order desc`` () =
    bresenham (0,10) (0,0) |> should be descending

[<Property>]
let ``No consecutive points should be 1 or 2 units greater`` (origin:int*int, destination:int*int) =
    bresenham origin destination
    |> Seq.pairwise
    |> Seq.map ((fun (a,b) -> b -- a) >> (uncurry (+)))
    |> Seq.forall (fun x -> x = 1 || x = 2)

[<Fact>]
let ``Rect should give 4 points for 0,0 to 1,1`` () =
    Rect.fromDimensions (0,0) 2 2
        |> List.ofSeq
        |> should haveLength 4

[<Fact>]
let ``Dimensions should be -1 from origin point`` () =
    (Rect.fromDimensions (0,0) 2 2) = (Rect.fromOppositeCorners (0,0) (1,1))

[<Fact>]
let ``Points should be x -> y`` () =
    (Rect.fromDimensions (0,0) 2 3)
    |> Seq.toList
    |> should equal [0,0; 0,1; 0,2; 1,0; 1,1; 1,2]

[<Fact>]
let ``Points should be x -> y y first`` () =
    (Rect.fromDimensions (0,0) 1 2)
    |> Seq.toList
    |> should equal [0,0; 0,1]

[<Fact>]
let ``Points should be x -> y x first`` () =
    (Rect.fromDimensions (0,0) 2 1)
    |> Seq.toList
    |> should equal [0,0; 1,0]

[<Property>]
let ``Rect should give unique points`` (o:int*int, l:int, h:int) =
    (Rect.fromDimensions o l h) |> should be unique

[<Property>]
let ``Rect should be within bounds for length`` (o: int*int, l:PositiveInt, h:PositiveInt) =
    let length = l.Get
    Rect.fromDimensions o length (h.Get)
        |> Seq.filter (fun (_,y) -> y = snd o)
        |> Seq.length
        |> ((=) length)

[<Property>]
let ``Rect should be within bounds for heights`` (o: int*int, l:PositiveInt, h:PositiveInt) =
    let height = h.Get
    Rect.fromDimensions o (l.Get) height
        |> Seq.filter (fun (x,_) -> x = fst o)
        |> Seq.length
        |> ((=) height)

[<Property>]
let ``Rect should have sensible area length * height`` (origin:int*int, l:PositiveInt, h:PositiveInt) =
    Rect.fromDimensions origin (l.Get) (h.Get)
    |> Seq.length
    |> ((=) (l.Get * h.Get))

[<Fact>]
let ``A radius 1 circle should just be the origin`` () =
    Circle.bresenham (1,1) 0
        |> should equal [1,1]

[<Fact>]
let ``A radius 2 circle should have 5 points`` () =
    Circle.bresenham (2,2) 2
        |> should haveLength 5