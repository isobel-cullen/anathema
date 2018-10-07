﻿module Anathema.TestHarness

open System
open System.Collections.Generic
open System.Collections.Immutable

open Anathema.Core
open Anathema.Core.Components
open Anathema.Core.Lenses
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Foundation
open Anathema.Core.Systems
open Terminal.Gui
open NStack
open Terminal.Gui
open Anathema.Core.Lenses

type KeyActionChooser = Key -> Action option

let (!!) (s: string) = ustring.Make s

let getView (state : WorldState) =
    state.Entities
            |> Seq.filter (fun kv -> kv.Value.IsEnabled)
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.choose (position)
            |> Seq.groupBy (fun p -> p.Coord)
            |> Seq.map (fun (xy, ps) ->
                let topSymbol =
                    ps
                    |> Seq.sortByDescending (fun p -> p.Layer)
                    |> Seq.head
                xy, topSymbol.Symbol)
            |> Map.ofSeq

let tryToChar k =
    if k >= int Char.MinValue && k <= int Char.MaxValue then
        Convert.ToChar k |> Some
    else None

let keyToDirection (key: KeyEvent) =
    match key.Key, tryToChar key.KeyValue with
    | _, Some 'h' | Key.CursorLeft, _   -> Direction.West  |> Some
    | _, Some 'j' | Key.CursorDown, _   -> Direction.South |> Some
    | _, Some 'k' | Key.CursorUp, _     -> Direction.North |> Some
    | _, Some 'l' | Key.CursorRight, _  -> Direction.East  |> Some
    | _, Some 'y'                       -> Direction.NorthWest |> Some
    | _, Some 'u'                       -> Direction.NorthEast |> Some
    | _, Some 'b'                       -> Direction.SouthWest |> Some
    | _, Some 'n'                       -> Direction.SouthEast |> Some
    | _, _                              -> None

type Arena (state: WorldState ref, setPlayerAction: Agency.PlayerCommand -> unit) =
    inherit View(Rect(0,1,60,20)) with
        override this.ProcessKey key =
            match key |> keyToDirection with
            | Some direction ->
                Agency.PlayerCommand.Dir direction |> setPlayerAction
                true
            | _ -> 
                match tryToChar (key.KeyValue) with
                | Some 'q' -> 
                    Application.RequestStop()
                    false
                | _ -> false

        override this.Redraw (rect) =
            base.Redraw(rect)

            let view = getView (! state)
            for x in 0 .. 59 do
                for y in 0 .. 19 do
                    match view.TryFind ((x,y)) with
                    | Some ch -> this.AddRune (x, y, Rune ch)
                    | _ -> this.AddRune (x, y, Rune '.')

        override this.CanFocus = true

let menu =
    MenuBar (
        [|
        MenuBarItem(!!"_Game",
            [|
                MenuItem(!!"_Quit", "",
                    fun () -> Application.RequestStop())
            |] )
        |]
    )

[<EntryPoint>]
let main argv =
    let schema = ColorScheme()
    schema.Normal <- Attribute(0)
    schema.Focus <- Attribute(0)
    schema.HotNormal <- Attribute(0)
    schema.HotFocus <- Attribute(0);

    let wall = Entity.Default
                    |> setPosition ({ Positionable.Default with Symbol = '#' })

    let door = Entity.Default
                |> setPosition ({ Positionable.Default with Symbol = '+' })
                |> setInteractable (Interactable.UnlockedDoor)
                |> Position.setCoords (10,10)

    let player = Entity.Default
                    |> setAgency (Agency.Player)
                    |> setPosition ({ Positionable.Actor with
                                        Coord = Point.zero
                                        Symbol = '@'
                                    })

    let monster = Entity.Default
                    |> setAgency (Agency.Wandering)
                    |> setPosition ({ Positionable.Actor with
                                        Coord = 13,13
                                        Symbol = 'm'
                                    })

    let monster2 = monster |> Position.setCoords (0,1)

    let mutable world = WorldState.Empty.Register player
    world <- world.Register monster
    world <- world.Register monster2

    for x in 10 .. 20 do
        for y in [5; 15] do
            world <- world.Register (wall |> Position.setCoords (x,y))

    for x in [10;20] do
        for y in 5 .. 15 do
            if y <> 10  || x <> 10 then
                world <- world.Register (wall |> Position.setCoords (x,y))

    world <- world.Register door

    let state = World world
    let stateLogger = StateAgent.getStateLogger()

    let rWorld = ref world

    Application.Init()
    let top = Application.Top

    let arena = Arena (rWorld, state.SetNextAction)
    let label = Label(60,24, "Label" |> ustring.Make)

    label.ColorScheme <- schema
    menu.ColorScheme <- schema

    top.Add arena
    top.Add label

    let clock = System.Diagnostics.Stopwatch.StartNew()
    let setState e =
        let nextState = state.Process()
        if Object.ReferenceEquals(! rWorld, nextState) |> not then
            clock.Restart()

            rWorld := nextState
            top.SetNeedsDisplay()
            stateLogger.Post nextState

            label.Text <- (sprintf "%d" clock.ElapsedMilliseconds |> ustring.Make)
            label.SetNeedsDisplay()

    Application.Iteration.Add setState
    Application.Run top
    0
