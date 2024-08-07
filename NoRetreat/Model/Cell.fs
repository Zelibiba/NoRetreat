namespace NoRetreat

open Elmish
open Avalonia.Input
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

    let selectAllCounters (tower: Tower) =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected >> not)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with IsSelected = true }

    let defineSelection (tower: Tower) =
        let selectedIdxs =
            tower.Counters
            |> Array.indexed
            |> Array.filter (snd >> _.IsSelected)
            |> Array.map fst

        { tower with
            IsSelected = Array.isEmpty selectedIdxs |> not },
        selectedIdxs

    let removeCounters idxs (tower: Tower) =
        let removed = Array.map tower.get_Item idxs
        let counters' = Array.except removed tower.Counters
        let tower' = if counters'.Length <= 1 then setIsExpanded false tower else tower

        { tower' with
            Counters = counters'
            IsSelected = false },
        removed

    let addCounters targetIdx newCounters (tower: Tower) =
        let counters' = 
            match targetIdx with
            | Some idx -> Array.insertManyAt (idx + 1) newCounters tower.Counters
            | None -> newCounters
        { tower with Counters = counters' }


type Cell =
    { CanBeSelected: bool
      Coords: Coordinates
      Tower: Tower }

module Cell =
    module Helpers =
        let setTower (cell: Cell) tower = { cell with Tower = tower }

    let init (coords, countersValue) =
        let counters = Array.init countersValue (ignore >> Counter.init)

        { CanBeSelected = false
          Coords = coords
          Tower = Tower.create counters }

    type Msg =
        | CounterMsg of int * Counter.Msg
        | ChangeTowerExpanded
        | Dropped
        | DeselectCounters
        | RemoveCounters of int array
        | AddCounters of int option * Counter array

    type ExtraData =
        | SelectedIdxs of int array
        | RemovedCounters of Counter array
        | NoData

    let update (msg: Msg) (state: Cell) =
        match msg with
        | CounterMsg(idx, counterMsg) ->
            match counterMsg with
            | Counter.ChangeSelection add when state.Tower.IsExpanded ->
                state.Tower
                |> if not add then Tower.deselectCounters else id
                |> Tower.updateCounter counterMsg idx
                |> Tower.defineSelection
                |> Tuple.map (Helpers.setTower state) SelectedIdxs
            | Counter.ChangeSelection _ ->
                Array.init state.Tower.Counters.Length id
                |> Array.iter (Tower.updateCounterAt state.Tower counterMsg)

                Tower.defineSelection state.Tower
                |> Tuple.map (Helpers.setTower state) SelectedIdxs
            | _ -> Tower.updateCounter counterMsg idx state.Tower |> Helpers.setTower state, NoData
        | ChangeTowerExpanded ->
            state.Tower
            |> Tower.deselectCounters
            |> Tower.setIsExpanded (not state.Tower.IsExpanded)
            |> Helpers.setTower state,
            NoData
        | Dropped -> state, NoData
        | DeselectCounters -> state.Tower |> Tower.deselectCounters |> Helpers.setTower state, NoData
        | RemoveCounters idxs ->
            state.Tower
            |> Tower.removeCounters idxs
            |> Tuple.map (Helpers.setTower state) RemovedCounters
        | AddCounters(idxOpt, counters) ->
            state.Tower
            |> Tower.addCounters idxOpt counters
            |> if state.Tower.IsExpanded then
                   id
               else
                   Tower.selectAllCounters
            |> Tower.defineSelection
            |> Tuple.map (Helpers.setTower state) SelectedIdxs


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
                  HexItem.zIndex 1

              if state.Tower.Counters.Length = 0 then
                  DragDrop.allowDrop true
                  DragDrop.onDrop (fun e ->
                      if e.Data.Contains(DataFormats.Counters) then dispatch Dropped)

              HexItem.content (
                  TowerPanel.create
                      [ TowerPanel.background "Pink"
                        //TowerPanel.height Counter.Size
                        //TowerPanel.width Counter.Size
                        TowerPanel.horizontalAlignment HorizontalAlignment.Center
                        TowerPanel.verticalAlignment VerticalAlignment.Center
                        if state.Tower.IsExpanded then
                            TowerPanel.deltaPadding (17.5, -38.5)
                        else
                            TowerPanel.deltaPadding (5, -11)
                        TowerPanel.onDoubleTapped (fun _ -> dispatch ChangeTowerExpanded)
                        TowerPanel.children (
                            state.Tower.Counters
                            |> Array.mapi (fun idx counter -> Counter.view counter (dispatchCounter idx))
                            |> Array.toList
                        ) ]
              ) ]
