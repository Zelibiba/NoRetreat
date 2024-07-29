namespace NoRetreat.Model

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open NoRetreat.Controls
open HexGameControls

type Tower =
    { UpdateField: bool
      Counters: Counter array
      IsExpanded: bool
      SelectedIdxs: int list }

    member this.Item
        with get index = this.Counters[index]
        and set index value = this.Counters[index] <- value

module Tower =
    let create counters =
        { UpdateField = false
          Counters = counters
          IsExpanded = false
          SelectedIdxs = [] }

    let update tower : Tower =
        { tower with
            UpdateField = not tower.UpdateField }

    let deselectCounters (tower: Tower) =
        tower.SelectedIdxs
        |> List.iter (fun idx -> tower[idx] <- Counter.update (Counter.SetIsSelected false) tower[idx])

        { tower with SelectedIdxs = [] }

    let selectCounter index (tower: Tower) =
        tower[index] <- Counter.update (Counter.SetIsSelected true) tower[index]

        { tower with
            SelectedIdxs = tower.SelectedIdxs @ [ index ] }

    let selectCounters indexes (tower: Tower) =
        indexes
        |> List.iter (fun index -> tower[index] <- Counter.update (Counter.SetIsSelected true) tower[index])

        { tower with
            SelectedIdxs = tower.SelectedIdxs @ indexes }

    let selectAllCounters (tower: Tower) =
        selectCounters [ 0 .. tower.Counters.Length - 1 ] tower

[<Struct>]
type Cell =
    { CanBeSelected: bool
      Coords: Coordinates
      Tower: Tower }

module Cell =

    type Msg =
        | CounterMsg of int * Counter.Msg
        | SetTowerExpanded of bool

    let init () =
        { CanBeSelected = false
          Coords = Coordinates.create (-1, -1)
          Tower = Tower.create [| Counter.init (); Counter.init (); Counter.init (); Counter.init () |] }

    let update (msg: Msg) (state: Cell) : Cell =
        match msg with
        | CounterMsg(idx, Counter.SetIsSelected true) when state.Tower.IsExpanded ->
            let tower' =
                state.Tower |> Tower.deselectCounters |> Tower.selectCounter idx |> Tower.update

            { state with Tower = tower' }
        | CounterMsg(idx, Counter.AddSelection true) when state.Tower.IsExpanded ->
            let tower' = state.Tower |> Tower.selectCounter idx |> Tower.update
            { state with Tower = tower' }
        | CounterMsg(_, Counter.SetIsSelected true)
        | CounterMsg(_, Counter.AddSelection true) ->
            let tower' = state.Tower |> Tower.selectAllCounters |> Tower.update
            { state with Tower = tower' }

        | CounterMsg(idx, counterMsg) ->
            let counter' = Counter.update counterMsg state.Tower.Counters[idx]
            state.Tower[idx] <- counter'

            { state with
                Tower = Tower.update state.Tower }
        | SetTowerExpanded isExpanded ->
            let tower' = Tower.deselectCounters state.Tower

            { state with
                Tower =
                    { tower' with
                        IsExpanded = isExpanded } }

    let view (state: Cell) (dispatch: Msg -> unit) : IView =
        TowerPanel.create
            [ TowerPanel.background "Pink"
              TowerPanel.deltaPadding (5, -11)
              TowerPanel.expandFactor 3.5
              TowerPanel.onIsExpandedChanged (SetTowerExpanded >> dispatch)
              TowerPanel.children (
                  state.Tower.Counters
                  |> Array.mapi (fun i c -> Counter.view c (fun cMsg -> (i, cMsg) |> CounterMsg |> dispatch))
                  |> Array.toList
              ) ]
