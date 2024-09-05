module NoRetreat.Field.Field

open NoRetreat

#nowarn "25"

open Elmish
open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.Input
open Avalonia.Media.Imaging
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls

open NoRetreat
open NoRetreat.Field
open NoRetreat.Field.Types

let init () =
    let field =
        HexData.hexes
        |> Dictionary
        |> Helpers.create
        |> Helpers.setCounters (-4, 0) [| 5 |]
        |> Helpers.setCounters (-5, 0) [| 2 |]
        |> Helpers.setCounters (-4, -1) [| 1 |]
        |> Helpers.setCounters (-4, 1) [| 51 |]
    (field, NotSelected), Cmd.none

type Msg = 
    | CellMsg of Coordinate * Cell.Msg
    | EndDragging

let doDrag e = async {
    let data = DataObject()
    data.Set(DataFormats.Counters, 0)
    let! _ = Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                    DragDrop.DoDragDrop(e, data, DragDropEffects.Move)) |> Async.AwaitTask
    return ()
    }

let update (msg: Msg) (state: Field, selection: SelectionState) =
    match msg, selection with
    | CellMsg(coord, Cell.CounterMsg(_, Counter.BeginDrag e)), Selected _ ->
        let counters = Cell.selectedCounters state[coord]
        let state' = 
            Helpers.updateCell (Cell.SetSelection CanBeDropped) coord state
            |> Movement.defineMovements coord counters
            
        (state', Dragging (coord, coord, counters)),
        Cmd.OfAsync.perform doDrag e (fun _ -> EndDragging)
    | EndDragging, Dragging (_,coord,_) ->
        (Movement.clearCellsSelection coord state |> Helpers.update, Selected coord), Cmd.none
    | CellMsg(coord, Cell.DragEntered),
      Dragging (origCoord, oldCoord, counters) when oldCoord <> coord ->
        let cost = state.get >> _.Terrain >> Terrain.cost 
        let    move (counter: Counter) =
            if Option.contains coord counter.MovedFrom
            then Counter.moveBackward
            else Counter.moveForward oldCoord
            <| cost <| counter
        let counters' = Array.map (move |> Counter.MoveCounter |> Counter.update) counters

        let state' =
            Movement.clearCellsSelection oldCoord state
            |> Movement.defineMovements coord counters'
            |> Helpers.updateCell (Cell.SetSelection CanBeDropped) coord
        (state',  Dragging (origCoord, coord, counters')), Cmd.none
    | CellMsg(coord, Cell.Dropped),
      (Dragging (oldCoord, _, counters) as selection) when oldCoord <> coord ->
        let state' =
            Movement.removeCounters oldCoord state
            |> Movement.addCounters coord counters
            |> Helpers.update
        (state', selection), Cmd.none
    | CellMsg(coord, cellMsg), _ ->
        let state' = Helpers.updateCell cellMsg coord state |> Helpers.update

        (match cellMsg with
        | Cell.CounterMsg(_, Counter.ChangeSelection _)
        | Cell.AddCounters _
        | Cell.LiftCounter _ ->
            match selection with
            | Selected loc when loc <> coord ->
                Helpers.updateCellAt state' Cell.DeselectCounters loc
            | _ -> ()
            let selection' =
                    if Array.isEmpty state'[coord].Tower.SelectedIDs
                    then NotSelected
                    else Selected coord
            state', selection'
        | _ -> state', selection),
        Cmd.none
        
            

let mapImage = Bitmap.create "avares://NoRetreat/Assets/Images/Map.jpg"

let view (state: Field, selection: SelectionState) (dispatch: Msg -> unit) : IView =
    let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

    ScrollPane.create
        [ ScrollPane.child (
              Panel.create
                  [ Panel.height 3365
                    Panel.width 5750
                    Panel.children
                        [ Image.create
                              [ Image.source mapImage
                                Image.stretch Avalonia.Media.Stretch.None ]
                          Canvas.create
                              [ Canvas.horizontalAlignment HorizontalAlignment.Stretch
                                Canvas.verticalAlignment VerticalAlignment.Stretch
                                Canvas.background "transparent"
                                DragDrop.allowDrop true
                                Canvas.focusable true
                                match selection with
                                | Selected coord ->
                                    Canvas.onKeyDown (fun e->
                                        e.Handled <- true
                                        match e.Key with
                                        | Key.Up -> dispatchCell coord (Cell.LiftCounter true)
                                        | Key.Down -> dispatchCell coord (Cell.LiftCounter false)
                                        | _ -> ()
                                    )
                                | _ -> ()
                                Canvas.children
                                    [ yield!
                                          state.Cells.Values
                                          |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coord))
                                      match selection with
                                      | Dragging(coord,_,_) ->
                                          yield MovableBorder.create
                                                [ MovableBorder.zIndex 10
                                                  MovableBorder.opacity 0.7
                                                  MovableBorder.child (
                                                      Cell.towerView
                                                          (Tower.selectedCounters state[coord].Tower|> Tower.create)
                                                          ignore
                                                  ) ]
                                      | _ -> ()
                                    ]
                              ]
                        ]
                  ]
          ) 
        ]
