namespace NoRetreat

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
open NoRetreat.Controls
open HexGameControls

[<Struct>]
type SelectionState =
    | NotSelected
    | Selected of coord: Coordinate
    | Dragging of origCoord: Coordinate * currentCoord: Coordinate * counters: Counter array

type Field =
    { UpdateValue: bool
      Cells: Dictionary<Coordinate, Cell> }

    member this.Item
        with get coord = this.Cells[coord]
        and set coord cell = this.Cells[coord] <- cell

module Field =
    module Helpers =
        let inline contains (field: Field) coord = field.Cells.ContainsKey coord

        let inline get (field: Field) coord = field[coord]

        let tryFind (field: Field) coord =
            match field.Cells.TryGetValue(coord) with
            | true, cell -> Some cell
            | false, _ -> None

        let inline unblockedDirections coord (field: Field) =
            Cell.unblockedDirections field[coord] |> Seq.filter (contains field)

        let inline unblockedDirsWithItself coord field =
            seq {
                yield! unblockedDirections coord field
                yield coord
            }

        let create dictionary =
            { UpdateValue = false
              Cells = dictionary }

        let update (field: Field) = { field with UpdateValue = not field.UpdateValue }

        let updateCell msg coord (field: Field) =
            field[coord] <- Cell.update msg field[coord]
            field

        let updateCellAt (field: Field) msg coord =
            field[coord] <- Cell.update msg field[coord]

        let setCounters rawCoord idxs (field: Field) =
            let coord = Coordinate.create rawCoord
            let counters = Array.map Counter.init idxs
            let owner = (CounterInfo.unbox counters[0].Info).Country

            field[coord] <- Tower.create counters |> Cell.setTower field[coord]
            unblockedDirsWithItself coord field 
            |> Seq.iter (updateCellAt field (Cell.AddZOC owner))
            field

        let removeCounters coord (field: Field) =
            let (Some owner) = field[coord].Tower.Owner
            unblockedDirsWithItself coord field 
            |> Seq.iter (updateCellAt field (Cell.SubZOC owner))

            updateCell (Cell.RemoveCounters) coord field

        let addCounters coord (counters: Counter array) (field: Field) =
            let owner = (CounterInfo.unbox counters[0].Info).Country
            unblockedDirsWithItself coord field
            |> Seq.iter (updateCellAt field (Cell.AddZOC owner))

            updateCell (Cell.AddCounters counters) coord field

        let clearCellsSelection coord field =
            unblockedDirsWithItself coord field
            |> Seq.iter (updateCellAt field (Cell.SetSelection Selection.NotSelected))
            field

        let defineMovements coord counters (field: Field) =
            let units = Array.map (_.Info >> CounterInfo.unbox) counters

            unblockedDirections coord field
            |> Seq.tuple (get field >> (fun cell ->
                Array.map (Cell.canMoveTo cell) units)
                >> Array.reduce (fun selection1 selection2 -> 
                    match selection1, selection2 with
                    | Selection.NotSelected, _
                    | _, Selection.NotSelected -> Selection.NotSelected
                    | MovedFrom, MovedFrom -> MovedFrom
                    | _, _ -> CanMoveTo))
            |> Seq.iter (fun (coord, selection) -> 
                updateCellAt field (Cell.SetSelection selection) coord)
            field


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
        match selection, msg with
        | Selected _, CellMsg(coord, Cell.CounterMsg(_, Counter.BeginDrag e)) ->
            let counters = Cell.selectedCounters state[coord]
            let state' = 
                Helpers.updateCell (Cell.SetSelection CanBeDropped) coord state
                |> Helpers.defineMovements coord counters
            
            (state', Dragging (coord, coord, counters)),
            Cmd.OfAsync.perform doDrag e (fun _ -> EndDragging)
        | Dragging (_,coord,_), EndDragging ->
            (Helpers.clearCellsSelection coord state |> Helpers.update, Selected coord), Cmd.none
        | Dragging (origCoord, oldCoord, counters),
          CellMsg(coord, Cell.DragEntered) when oldCoord <> coord ->
            let cost loc = Terrain.cost state[loc].Terrain
            let move (unit: Unit)=
                if Option.contains coord unit.MovedFrom
                then Unit.moveBackward (cost oldCoord)
                else Unit.moveForward oldCoord (cost coord)
                <| unit
            let counters' = Array.map (move |> Counter.UpdateUnit |> Counter.update) counters

            let state' =
                Helpers.clearCellsSelection oldCoord state
                |> Helpers.defineMovements coord counters'
                |> Helpers.updateCell (Cell.SetSelection CanBeDropped) coord
            (state',  Dragging (origCoord, coord, counters')), Cmd.none
        | Dragging (oldCoord, _, counters) as selection,
          CellMsg(coord, Cell.Dropped) when oldCoord <> coord ->
            let state' =
                Helpers.removeCounters oldCoord state
                |> Helpers.addCounters coord counters
                |> Helpers.update
            (state', selection), Cmd.none
        | selection, CellMsg(coord, cellMsg) ->
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
