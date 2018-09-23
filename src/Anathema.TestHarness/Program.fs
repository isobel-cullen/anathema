﻿module Anathema.TestHarness

open System
open System.Collections.Generic
open System.Collections.Immutable

open Anathema.Core
open Anathema.Core.Actions
open Anathema.Core.Components
open Anathema.Core.Entities
open Anathema.Core.Lenses
open Anathema.Core.FrameworkExtensions
open Anathema.Core.Foundation
open Terminal.Gui

type KeyActionChooser = Key -> Action option

let getView (state : WorldState) =
    state.Entities
            |> Seq.filter (fun kv -> kv.Value.IsEnabled)
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.choose2 (position) (visibility)
            |> Seq.map (fun (p,v) -> KeyValuePair((p.X, p.Y), v.Symbol))
            |> ImmutableDictionary.CreateRange

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

type Arena (state: WorldState ref, setPlayerAction: Action -> unit) =
    inherit View(Rect(0,0,80,25)) with
        override this.ProcessKey key =
            match key |> keyToDirection with
            | Some direction ->
                { 
                    EntityId = 0L;
                    Cost = 50
                    Type = Move direction
                } |> setPlayerAction
                true
            | _ -> 
                match tryToChar (key.KeyValue) with
                | Some 'q' -> 
                    Application.RequestStop()
                    false
                | _ -> false

        override this.Redraw (rect) =
            this.Clear()
            let view = getView (! state)
            for x in 0 .. 79 do
                for y in 0 .. 24 do
                    match view.TryGetValue ((x,y)) with 
                    | true, ch -> this.AddRune (x, y, Rune ch)
                    | false, _ -> this.AddRune (x, y, Rune '.')
            ()

        override this.CanFocus = true

[<EntryPoint>]
let main argv =

    let player = Entity.Empty
                    |> setAgency (Agency.Player)
                    |> setPosition Point.One
                    |> setVisibility {Symbol = '@'}

    let monster = Entity.Empty
                    |> setAgency (Agency.Wandering)
                    |> setPosition ({X=10;Y=15})
                    |> setVisibility {Symbol='m'}

    let monster2 = { monster with Position = Some {X=2;Y=1}}

    let mutable world = WorldState.Empty.Register player
    world <- world.Register monster
    world <- world.Register monster2

    let state = World world
    let stateLogger = StateAgent.getStateLogger()

    let rWorld = ref world
    let arena = Arena (rWorld, state.SetNextAction) 

    Application.Init()
    let top = Application.Top
    top.Add arena

    let setState e =
        let nextState = state.Process()
        if Object.ReferenceEquals(! rWorld, nextState) |> not then
            rWorld := nextState
            top.SetNeedsDisplay()
            stateLogger.Post nextState

    Application.Iteration.Add setState
    Application.Run top
    0
