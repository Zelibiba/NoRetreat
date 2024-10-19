namespace NoRetreat.Game.Field
open NoRetreat
open NoRetreat.Game
open NoRetreat.Game.Field

//#nowarn "25"

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
    | CheckSupply of firstCountry: Country
    | SetMask of Cell.Mask
    | NextMaskOption of Cell.Mask
    | Test

module Main =
    let init () =
        let field =
            HexData.hexes
            |> T
            |> Helpers.setCounters (-4, 0)  [| 5 |]
            |> Helpers.setCounters (-5, 0)  [| 2; 1 |]
            //|> Helpers.setCounters (-4, -1) [| 1 |]
            |> Helpers.setCounters (-7, 10) [| 4 |]
            |> Helpers.setCounters (-4, 1)  [| 51 |]
            |> Helpers.setCounters (-1, -2) [| 52 |]
            |> Helpers.setCounters (-9, 11) [| 55 |]
        field, Cmd.none

    let private doDrag e = async {
        let data = DataObject()
        data.Set(DataFormats.Counters, 0)
        let! _ = Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                        DragDrop.DoDragDrop(e, data, DragDropEffects.Move)) |> Async.AwaitTask
        return ()
        }

    let update (msg: Msg) (state: T) =
        match msg, state.Selection with
        | CellMsg(coord, Cell.TowerMsg(Tower.CounterMsg(_, Counter.BeginDrag e))), Selected _ ->
            let counters = Cell.selectedCounters state[coord]

            state.setMask Cell.NoMask
            |> Movement.defineMovements coord counters
            |> Helpers.setSelection (Dragging (coord, coord, counters)),

            Cmd.OfAsync.perform doDrag e (fun _ -> EndDragging)
        | EndDragging, Dragging (_,coord,_) ->
            Movement.clearCellsSelection coord state
            |> Helpers.setSelection (Selected coord), Cmd.none
        | CellMsg(coord, Cell.DragEntered),
          Dragging (origCoord, oldCoord, counters) when oldCoord <> coord ->
            let cost unitType = state.getCell >> _.Terrain >> Cell.Terrain.cost unitType
            let counters' = Array.map (Counter.update <| Counter.MoveCounter (cost, coord)) counters

            Movement.clearCellsSelection oldCoord state
            |> Movement.defineMovements coord counters'
            |> Helpers.setSelection (Dragging (origCoord, coord, counters')), Cmd.none
        | CellMsg(coord, Cell.Dropped),
          Dragging (oldCoord, _, counters) when oldCoord <> coord ->
            Movement.removeCounters oldCoord state
            |> Movement.addCounters coord counters, Cmd.none
        | CellMsg(coord, Cell.TowerMsg(Tower.CounterMsg(idx, Counter.ChangeSelection add))), _ ->
            let state' = 
                Helpers.updateTowerAt (Tower.CounterMsg(idx, Counter.ChangeSelection add)) coord state
                |> match state.Selection with
                   | Selected loc when loc <> coord ->
                       Helpers.updateTowerAt Tower.DeselectCounters loc
                   | _ -> id

            state'.setSelection (
                if Array.isEmpty state'[coord].Tower.SelectedIdxs
                then NotSelected
                else Selected coord),
            Cmd.none
        | CellMsg(coord, cellMsg), _ ->
            Helpers.updateCellAt cellMsg coord state, Cmd.none
        | CheckSupply firstCountry, _ ->
            [firstCountry; firstCountry.Opposite]
            |> List.fold Supply.checkSupply state,
            Cmd.none
        | Test, _ -> 
            [Germany; USSR]
            |> List.fold Supply.checkSupply state,
            Cmd.none
        | SetMask mask, _ -> 
            if mask = Cell.SupplyMask && state.Mask <> mask
            then List.fold (Supply.calculateDirectSupply) state [USSR; Germany]
            else state
            |> Helpers.setMask mask, Cmd.none
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
            state.setMaskOptions maskOptions, cmd


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
                    yield! state.Cells |> Seq.toArray
                        |> Array.map (fun cell -> Cell.view (cell, maskInfo) (dispatchCell cell.Coord))
                    match state.Selection with
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

    let view (state: T) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        DockPanel.create [
            DockPanel.focusable true
            match state.Selection with
            | Selected coord ->
                DockPanel.onKeyDown (fun e->
                    e.Handled <- true
                    match e.Key with
                    | Key.Up -> dispatchCell coord (Cell.TowerMsg (Tower.LiftCounter true))
                    | Key.Down -> dispatchCell coord (Cell.TowerMsg (Tower.LiftCounter false))
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
                        maskButton (Cell.ZOCMask, "ZOC", Key.Z) state dispatch
                        maskButton (Cell.SupplyMask, "Supply", Key.S) state dispatch
                        Button.create [
                            Button.content "Test"
                            Button.onClick (fun _ -> dispatch Test)
                        ]
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