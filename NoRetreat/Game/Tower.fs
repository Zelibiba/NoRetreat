module NoRetreat.Game.Tower
open NoRetreat

open Avalonia.Layout
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

open HexGameControls

type T =
    private { _Counters: Counter.T list
              IsExpanded: bool }

    member x.get index = x._Counters[index]
    //member x.Item with get index = x._Counters[index]
    member x.Counters = x._Counters

    member x.Height = x._Counters.Length
    member x.Indexes = [| 0 .. x.Height - 1 |]

    member x.SelectedIdxs = Array.filter (x.get >> _.IsSelected) x.Indexes
    member x.NotSelectedIdxs = Array.filter (x.get >> _.IsSelected >> not) x.Indexes

    member x.Owner = x._Counters |> List.tryPick (_.Country >> Some)

module private Helpers =
    let create counters =
        { _Counters = Array.toList counters
          IsExpanded = false }

    let inline setIsExpanded isExpanded tower = { tower with IsExpanded = isExpanded }

    let updateCounter msg idx (tower: T) =
        let counter' = Counter.update msg tower._Counters[idx]
        { tower with _Counters = List.updateAt idx counter' tower._Counters}

    let updateCounters msg tower indexes =
        Array.foldBack (updateCounter msg) indexes tower

    let deselectCounters (tower: T) =
        updateCounters (Counter.ChangeSelection false) tower tower.SelectedIdxs

    let selectAllCounters (tower: T) =
        updateCounters (Counter.ChangeSelection false) tower tower.NotSelectedIdxs

    let removeCounters counters (tower: T) =
        let set1 = Set.ofArray counters
        let set2 = Set.ofList tower._Counters

        if not <| Set.isSubset set1 set2 then
            failwith "Can't remove counter that not in tower"

        let counters'Set = Set.difference set2 set1
        let tower' = { tower with _Counters = Set.toList counters'Set }
        
        if counters'Set.Count <= 1
        then setIsExpanded false tower'
        else tower'

    let addCounters newCounters tower =
        { tower with _Counters = tower._Counters @ Array.toList newCounters }

    let private swap (array: _ array) i j =
        let tmp = array[i]
        array[i] <- array[j]
        array[j] <- tmp

    let liftCountersUp (tower: T) =
        let lastIdx = tower.Height - 1
        let counters = List.toArray tower._Counters
        Array.rev tower.SelectedIdxs
        |> Array.iter (fun i ->
            if i < lastIdx && not counters[i+1].IsSelected then swap counters (i+1) i)

        { tower with _Counters = Array.toList counters }

    let liftCountersDown (tower: T) =
        let counters = List.toArray tower._Counters
        Array.iter (fun i ->
            if i > 0 && not counters[i-1].IsSelected then swap counters (i-1) i)
            tower.SelectedIdxs

        { tower with _Counters = Array.toList counters }


open Helpers


let selectedCounters (tower: T) = Array.map tower.get tower.SelectedIdxs

let init = create

type Msg =
    | CounterMsg of int * Counter.Msg
    | Init of Counter.T array
    | UpdateAllCounters of Counter.Msg
    | ChangeExpansion
    | DeselectCounters
    | RemoveCounters of Counter.T array
    | AddCounters of Counter.T array
    | LiftCounter of up: bool


let update (msg: Msg) (state: T) : T =
    match msg with
    | CounterMsg (idx, Counter.ChangeSelection add) when state.IsExpanded ->
        if not add then deselectCounters state else state
        |> updateCounter (Counter.ChangeSelection add) idx
    | CounterMsg (_, (Counter.ChangeSelection _ as counterMsg)) -> updateCounters counterMsg state state.Indexes
    | CounterMsg (idx, counterMsg) -> updateCounter counterMsg idx state

    | Init counters -> init counters

    | UpdateAllCounters counterMsg -> updateCounters counterMsg state state.Indexes
    | DeselectCounters -> deselectCounters state
    | ChangeExpansion -> deselectCounters state |> setIsExpanded (not state.IsExpanded)

    | RemoveCounters counters -> removeCounters counters state
    | AddCounters counters -> addCounters counters state |> if state.IsExpanded then id else selectAllCounters

    | LiftCounter up -> state |> if up then liftCountersUp else liftCountersDown


let private toolTipView (counters: Counter.T list) =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            counters
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
                                if counter.Markers = Counter.OutOfSupply then
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
            state._Counters
            |> List.mapi (fun idx counter -> Counter.view counter (dispatchCounter idx))) 
        ToolTip.placement PlacementMode.BottomEdgeAlignedRight
        ToolTip.showDelay 1000
        ToolTip.tip (toolTipView state._Counters)
    ]
