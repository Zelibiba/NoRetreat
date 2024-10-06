namespace NoRetreat.Game.Field
open NoRetreat
open NoRetreat.Game
open NoRetreat.Game.Field

#nowarn "25"

open Elmish
open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.Input
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls

type Msg = 
    | CellMsg of Coordinate * Cell.Msg
    | EndDragging
    | SetMask of Cell.Mask
    | NextMaskOption of Cell.Mask
    | Test

module Main =
    let init () =
        let field =
            HexData.hexes
            |> Dictionary
            |> Helpers.create
            |> Helpers.setCounters (-4, 0)  [| 5 |]
            |> Helpers.setCounters (-5, 0)  [| 2; 1 |]
            //|> Helpers.setCounters (-4, -1) [| 1 |]
            |> Helpers.setCounters (-7, 11) [| 4 |]
            |> Helpers.setCounters (-4, 1)  [| 51 |]
            |> Helpers.setCounters (-1, -2) [| 52 |]
        field, Cmd.none

    let private doDrag e = async {
        let data = DataObject()
        data.Set(DataFormats.Counters, 0)
        let! _ = Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                        DragDrop.DoDragDrop(e, data, DragDropEffects.Move)) |> Async.AwaitTask
        return ()
        }

    let update (msg: Msg) (state: T) =
        match msg, state.CounterSelection with
        | CellMsg(coord, Cell.TowerMsg(Tower.CounterMsg(_, Counter.BeginDrag e))), Selected _ ->
            let counters = Cell.selectedCounters state[coord]

            match state.Mask with
            | Cell.NoMask -> state
            | _ -> { state with Mask = Cell.NoMask }
            |> Helpers.updateCell (Cell.SetSelection Cell.CanBeDropped) coord
            |> Movement.defineMovements coord counters
            |> Helpers.setSelection (Dragging (coord, coord, counters)),

            Cmd.OfAsync.perform doDrag e (fun _ -> EndDragging)
        | EndDragging, Dragging (_,coord,_) ->
            Movement.clearCellsSelection coord state
            |> Helpers.setSelection (Selected coord), Cmd.none
        | CellMsg(coord, Cell.DragEntered),
          Dragging (origCoord, oldCoord, counters) when oldCoord <> coord ->
            let cost = state.get >> _.Terrain >> Cell.Terrain.cost 
            let move (counter: Counter.T) =
                if Option.contains coord counter.MovedFrom
                then Counter.moveBackward
                else Counter.moveForward oldCoord
                <| cost <| counter
            let counters' = Array.map (move |> Counter.MoveCounter |> Counter.update) counters

            Movement.clearCellsSelection oldCoord state
            |> Movement.defineMovements coord counters'
            |> Helpers.updateCell (Cell.SetSelection Cell.CanBeDropped) coord
            |> Helpers.setSelection (Dragging (origCoord, coord, counters')), Cmd.none
        | CellMsg(coord, Cell.Dropped),
          Dragging (oldCoord, _, counters) when oldCoord <> coord ->
            Movement.removeCounters oldCoord state
            |> Movement.addCounters coord counters
            |> Helpers.update, Cmd.none
        | CellMsg(coord, cellMsg), _ ->
            let state' = Helpers.updateCell cellMsg coord state |> Helpers.update

            (match cellMsg with
            | Cell.TowerMsg (Tower.CounterMsg(_, Counter.ChangeSelection _))
            | Cell.TowerMsg (Tower.AddCounters _)
            | Cell.TowerMsg (Tower.LiftCounter _) ->
                match state.CounterSelection with
                | Selected loc when loc <> coord ->
                    Helpers.updateTowerAt state' (Tower.DeselectCounters) loc
                | _ -> ()

                state
                |> Helpers.setSelection (
                    if Array.isEmpty state'[coord].Tower.SelectedIDs
                    then NotSelected
                    else Selected coord)
            | _ -> state'),
            Cmd.none
        | Test, _ ->
            List.iter (Supply.defineUnitsSupply state) [USSR; Germany]
            (Helpers.update state), Cmd.none
        | SetMask mask, _ -> 
            if mask = Cell.SupplyMask && state.Mask <> mask then
                List.iter (Supply.calculateDirectSupply state) [USSR; Germany]
            { state with Mask = mask }, Cmd.none
        | NextMaskOption mask, _ ->
            let countryOpt' = 
                match state.MaskOptions[mask] with
                | None -> Some USSR
                | Some USSR -> Some Germany
                | Some Germany -> None
            let maskOptions = Map.add mask countryOpt' state.MaskOptions 
            let cmd = 
                if state.Mask = mask 
                then Cmd.ofMsg (SetMask mask)
                else Cmd.none
            { state with MaskOptions = maskOptions }, cmd


    module private Images =
        let map = Bitmap.create "avares://NoRetreat/Assets/Images/Map.jpg"
        let USSRIcon = Bitmap.create "avares://NoRetreat/Assets/Icons/USSR.png"
        let GermanyIcon = Bitmap.create "avares://NoRetreat/Assets/Icons/Germany.png"
        let USSRGermanyIcon = Bitmap.create "avares://NoRetreat/Assets/Icons/Both.png"

    let maskButton (mask: Cell.Mask, label: string, key: Key) (state: T) (dispatch: Msg -> unit) : IView =
        ToggleButton.create [
            ToggleButton.padding (5, 2)
            ToggleButton.onChecked (fun _ -> dispatch <| SetMask mask)
            ToggleButton.onUnchecked (
                (fun _ -> if state.Mask = mask then dispatch <| SetMask Cell.NoMask),
                SubPatchOptions.OnChangeOf state.Mask)
            ToggleButton.hotKey <| KeyGesture(key)
            ToggleButton.isChecked (state.Mask = mask)
            ToggleButton.content (
                WrapPanel.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    WrapPanel.children [
                        TextBlock.create [
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.text label
                        ]
                        Button.create [
                            Button.borderBrush "black"
                            Button.borderThickness 1
                            Button.margin (5, 0, 0, 0)
                            Button.padding 2
                            Button.background "transparent"
                            Button.onClick (fun _ -> dispatch <| NextMaskOption mask)
                            Button.content (
                                Image.createWith (
                                    match state.MaskOptions[mask] with
                                    | None -> Images.USSRGermanyIcon
                                    | Some USSR -> Images.USSRIcon
                                    | Some Germany -> Images.GermanyIcon
                                )
                            )
                        ]
                    ]
                ]
            )
        ]

    let viewField (state: T) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        let maskInfo = Cell.MaskInfo (state.Mask, state.MaskOptions[state.Mask])

        ScrollPane.create [ ScrollPane.child (
            Canvas.create [
                Canvas.height 3365
                Canvas.width 5750
                Canvas.background (
                    let brush = ImageBrush Images.map
                    brush.Stretch <- Stretch.UniformToFill
                    brush
                )
                DragDrop.allowDrop true
            
                Canvas.children [
                    yield! state.Cells.Values
                            |> Seq.map (fun cell -> Cell.view (cell, maskInfo) (dispatchCell cell.Coord))
                    match state.CounterSelection with
                    | Dragging (coord, _, _) ->
                        yield MovableBorder.create [ 
                            MovableBorder.zIndex 10
                            MovableBorder.opacity 0.7
                            MovableBorder.child (
                                Tower.view
                                    (Tower.selectedCounters state[coord].Tower|> Tower.init)
                                    ignore
                            ) ]
                    | _ -> ()
                ]
            ]
        ) ]

    let view (state: T, phase: Phase) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        DockPanel.create [
            DockPanel.focusable true
            match state.CounterSelection with
            | Selected coord ->
                DockPanel.onKeyDown (fun e->
                    e.Handled <- true
                    match e.Key with
                    | Key.Up -> dispatchCell coord (Cell.TowerMsg (Tower.LiftCounter true))
                    | Key.Down -> dispatchCell coord (Cell.TowerMsg (Tower.LiftCounter false))
                    | Key.T -> dispatch Test
                    | _ -> ()
                )
            | _ -> ()
            DockPanel.children [
                StackPanel.create [
                    DockPanel.dock Dock.Top
                    StackPanel.zIndex 1
                    StackPanel.background "white"
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.height 40
                    StackPanel.margin 5
                    StackPanel.children [
                        Border.create [
                            Border.width 200
                            Border.borderBrush "black"
                            Border.borderThickness 1
                            Border.child (TextBlock.create [
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                TextBlock.verticalAlignment VerticalAlignment.Center
                                TextBlock.text <| sprintf "%A" phase
                            ])
                        ]
                        maskButton (Cell.ZOCMask, "ZOC", Key.Z) state dispatch
                        maskButton (Cell.SupplyMask, "Supply", Key.S) state dispatch
                    ]
                ]
                Border.create [ 
                    Border.borderBrush "black"
                    Border.borderThickness 5
                    Border.child (
                        viewField state dispatch
                ) ]
            ]
        ]