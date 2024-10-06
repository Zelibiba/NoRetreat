module NoRetreat.Game.Tower
open NoRetreat

open Avalonia.Layout
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open HexGameControls

type T =
    { Counters: Counter.T array
      SelectedIDs: int array
      IsExpanded: bool }

    member x.get index = x.Counters[index]
    member x.Item
        with get index = x.Counters[index]
        and set index value = x.Counters[index] <- value

    member x.Height = x.Counters.Length

    member x.Indexes = [| 0 .. x.Height - 1 |]

    member x.Owner =
        x.Counters |> Array.tryPick (_.Country >> Some)

module private Helpers =
    let create counters =
        { Counters = counters
          SelectedIDs = [||]
          IsExpanded = false }

    //let get (tower: T) idx = tower[idx]

    let setIsExpanded isExpanded tower = { tower with IsExpanded = isExpanded }

    let updateCounter msg idx (tower: T) =
        tower[idx] <- Counter.update msg tower[idx]
        tower

    let updateCounterAt (tower: T) msg idx =
        tower[idx] <- Counter.update msg tower[idx]

    let deselectCounters tower =
        Array.iter (updateCounterAt tower (Counter.ChangeSelection false)) tower.SelectedIDs

        { tower with SelectedIDs = [||] }

    let selectAllCounters tower =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected >> not)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with SelectedIDs = tower.Indexes }

    let defineSelection (tower: T) =
        let selectedIdxs =
            Array.filter (tower.get >> _.IsSelected) tower.Indexes

        { tower with SelectedIDs = selectedIdxs }    

    let removeSelectedCounters (tower: T) =
        let counters' =
            Array.except tower.SelectedIDs tower.Indexes
            |> Array.map tower.get

        let tower' =
            if counters'.Length <= 1 then
                setIsExpanded false tower
            else
                tower

        { tower' with
            Counters = counters'
            SelectedIDs = [||] }

    let addCounters newCounters tower =
        { tower with
            Counters = Array.append tower.Counters newCounters }

    let liftCounterUp tower =
        Array.sortDescending tower.SelectedIDs
        |> fun array -> if array[0] = tower.Height - 1 then array[1..] else array
        |> Array.iter (fun idx ->
            let counter = tower[idx]
            tower[idx] <- tower[idx + 1]
            tower[idx + 1] <- counter)

        tower

    let liftCounterDown tower =
        Array.sort tower.SelectedIDs
        |> fun array -> if array[0] = 0 then array[1..] else array
        |> Array.iter (fun idx ->
            let counter = tower[idx]
            tower[idx] <- tower[idx - 1]
            tower[idx - 1] <- counter)

        tower

open Helpers


let selectedCounters (tower: T) =
    Array.map tower.get tower.SelectedIDs

let init = create

type Msg =
    | CounterMsg of int * Counter.Msg
    | UpdateAllCounters of Counter.Msg
    | ChangeExpansion
    | DeselectCounters
    | RemoveCounters
    | AddCounters of Counter.T array
    | LiftCounter of up: bool


let update (msg: Msg) (state: T) : T =
    match msg with
    | CounterMsg (idx, Counter.ChangeSelection add) when state.IsExpanded ->
        if not add then deselectCounters state else state
        |> updateCounter (Counter.ChangeSelection add) idx
        |> defineSelection
    | CounterMsg (_, (Counter.ChangeSelection _ as counterMsg)) ->
        Array.iter (updateCounterAt state counterMsg) state.Indexes
        defineSelection state
    | CounterMsg (idx, counterMsg) -> updateCounter counterMsg idx state
    | UpdateAllCounters counterMsg ->
        Array.iter (updateCounterAt state counterMsg) state.Indexes
        state
    | DeselectCounters -> deselectCounters state
    | ChangeExpansion ->
        deselectCounters state
        |> setIsExpanded (not state.IsExpanded)
    | RemoveCounters -> removeSelectedCounters state
    | AddCounters counters ->
        addCounters counters state
        |> if state.IsExpanded then id else selectAllCounters
        |> defineSelection
    | LiftCounter up ->
        state
        |> if up then liftCounterUp else liftCounterDown
        |> defineSelection

let private toolTipView (counters: Counter.T array) =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            counters |> Array.toList
            |> List.map (fun counter ->
                Border.create [
                    Border.borderBrush "black"
                    Border.borderThickness 1
                    Border.margin 10
                    Border.padding 2
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                Counter.view counter ignore
                                if counter.Buff = Counter.OutOfSupply then
                                    Counter.ImageOOS()
                            ]
                        ]
                    )
                ]
            )
        )
    ]

let view (state: T) (dispatch: Msg -> unit) : IView =
    let dispatchCounter = Library.dispatchwithIndex dispatch CounterMsg

    TowerPanel.create [ 
        TowerPanel.horizontalAlignment HorizontalAlignment.Center
        TowerPanel.verticalAlignment VerticalAlignment.Center
        if state.IsExpanded then
            TowerPanel.deltaPadding (17.5, -38.5)
        else
            TowerPanel.deltaPadding (5, -11)
        TowerPanel.onDoubleTapped (fun _ -> dispatch ChangeExpansion)
        TowerPanel.children (
            state.Counters
            |> Array.mapi (fun idx counter -> Counter.view counter (dispatchCounter idx))
            |> Array.toList) 
        ToolTip.placement PlacementMode.BottomEdgeAlignedRight
        ToolTip.showDelay 1000
        ToolTip.tip (toolTipView state.Counters)
    ]
