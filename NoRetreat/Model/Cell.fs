namespace NoRetreat

open Elmish
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open NoRetreat.Controls
open HexGameControls

type Tower =
    { UpdateField: bool
      Counters: Counter array
      IsExpanded: bool
      IsSelected: bool }

    member this.Item
        with get index = this.Counters[index]
        and set index value = this.Counters[index] <- value

    member this.Update() =
        { this with
            UpdateField = not this.UpdateField }

module Tower =
    let create counters =
        { UpdateField = false
          Counters = counters
          IsExpanded = false
          IsSelected = false }

    let update (tower: Tower) = tower.Update()

    let setIsExpanded isExpanded tower = { tower with IsExpanded = isExpanded }

    let updateCounter msg idx (tower: Tower) =
        tower[idx] <- Counter.update msg tower[idx]
        tower

    let updateCounterAt tower msg idx = updateCounter msg idx tower |> ignore

    let deselectCounters (tower: Tower) =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with IsSelected = false }


    let defineSelection (tower: Tower) =
        let selectedIdxs =
            tower.Counters
            |> Array.indexed
            |> Array.filter (snd >> _.IsSelected)
            |> Array.map fst

        { tower with
            IsSelected = Array.isEmpty selectedIdxs |> not },
        selectedIdxs


type Cell =
    { CanBeSelected: bool
      Coords: Coordinates
      Tower: Tower }

module Cell =

    let setTower (cell: Cell) tower = { cell with Tower = tower }

    let init (coords, countersValue) =
        let counters = Array.init countersValue (ignore >> Counter.init)

        { CanBeSelected = false
          Coords = coords
          Tower = Tower.create counters }

    type Msg =
        | CounterMsg of int * Counter.Msg
        | DeselectCounters
        | SetTowerExpanded of bool

    type SelectedIdxs =
        | Selected of int array
        | NoSelection

    let update (msg: Msg) (state: Cell) =
        match msg with
        | CounterMsg(idx, counterMsg) ->
            match counterMsg with
            | Counter.ChangeSelection add when state.Tower.IsExpanded ->
                state.Tower
                |> if not add then Tower.deselectCounters else id
                |> Tower.updateCounter counterMsg idx
                |> Tower.defineSelection
                |> Tuple.map (setTower state) Selected
            | Counter.ChangeSelection _ ->
                Array.init state.Tower.Counters.Length id
                |> Array.iter (Tower.updateCounterAt state.Tower counterMsg)

                Tower.defineSelection state.Tower |> Tuple.map (setTower state) Selected
            | _ -> Tower.updateCounter counterMsg idx state.Tower |> setTower state, NoSelection
        | DeselectCounters -> state.Tower |> Tower.deselectCounters |> setTower state, NoSelection
        | SetTowerExpanded isExpanded ->
            state.Tower
            |> Tower.deselectCounters
            |> Tower.setIsExpanded isExpanded
            |> setTower state,
            NoSelection


    let diagonal = 60.

    let computeX coords =
        1.73205080756 * diagonal * (float coords.C + (float coords.R) / 2.)

    let computeY coords = 1.5 * diagonal * (float coords.R)

    let view (state: Cell) (dispatch: Msg -> unit) : IView =
        let dispatchCounter = Library.dispatchwithIndex dispatch CounterMsg

        HexItem.create
            [ HexItem.diagonal diagonal
              HexItem.left (computeX state.Coords)
              HexItem.top (computeY state.Coords)
              HexItem.background "transparent"
              HexItem.backgroundOpacity 1
              HexItem.borderThickness 3
              HexItem.borderBrush "black"
              HexItem.clipToBounds false
              if state.Tower.IsSelected then
                  HexItem.zIndex 1000
              HexItem.content (
                  TowerPanel.create
                      [ TowerPanel.background "Pink"
                        //TowerPanel.height Counter.Size
                        //TowerPanel.width Counter.Size
                        TowerPanel.horizontalAlignment HorizontalAlignment.Center
                        TowerPanel.verticalAlignment VerticalAlignment.Center
                        TowerPanel.deltaPadding (5, -11)
                        TowerPanel.expandFactor 3.5
                        TowerPanel.onIsExpandedChanged (SetTowerExpanded >> dispatch)
                        TowerPanel.children (
                            state.Tower.Counters
                            |> Array.mapi (fun idx counter -> Counter.view counter (dispatchCounter idx))
                            |> Array.toList
                        ) ]
              ) ]
