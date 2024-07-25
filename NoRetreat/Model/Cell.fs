module NoRetreat.Model.Cell

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open NoRetreat.Controls
open NoRetreat.Controls.CountersPanel
open HexGameControls

type State = 
    {
    CanBeSelected: bool
    Coords: Coordinates
    Counters: Counter.State list
    }

type Msg = 
    | CounterMsg of Counter.Msg

let init () =
    {
    CanBeSelected = false
    Coords = Coordinates.create (-1,-1)
    Counters = List.replicate 4 (Counter.init())
    }

let update (msg: Msg) (state: State) : State =
    match msg with
    | CounterMsg counterMsg -> state

let view (state: State) (dispatch: Msg -> unit) : IView =
    CountersPanel.create [
        TowerPanel.deltaPadding (5, -11)
        TowerPanel.expandFactor 3.5
        TowerPanel.children (
            state.Counters
            |> List.map (fun c -> Counter.view c (CounterMsg >> dispatch))
        )
    ]