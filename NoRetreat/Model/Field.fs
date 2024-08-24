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


type DraggingState =
    { Counters: Counter array
      TargetCoord: Coordinate }

[<Struct>]
type SelectionState =
    { Location: Coordinate
      DraggingState: DraggingState option }

type Field =
    { UpdateValue: bool
      Cells: Dictionary<Coordinate, Cell>
      SelectionState: SelectionState option }

    member this.Item
        with get coord = this.Cells[coord]
        and set coord cell = this.Cells[coord] <- cell

module Field =
    module Helpers =
        let contains (field: Field) coord = field.Cells.ContainsKey coord

        let get (field: Field) coord = field[coord]

        let tryFind (field: Field) coord =
            match field.Cells.TryGetValue(coord) with
            | true, cell -> Some cell
            | false, _ -> None

        let unblockedDirections coord (field: Field) =
            Cell.unblockedDirections field[coord] |> Seq.filter (contains field)

        let unblockedDirsWithItself coord field =
            seq {
                yield! unblockedDirections coord field
                yield coord
            }

        let create dictionary =
            { UpdateValue = false
              Cells = dictionary
              SelectionState = None }

        let update (field: Field) = { field with UpdateValue = not field.UpdateValue }

        let updateCell msg coord (field: Field) =
            field[coord] <- Cell.update msg field[coord]
            field

        let updateCellAt (field: Field) msg coord =
            field[coord] <- Cell.update msg field[coord]

        let setDraggingState selectionState draggingStateOpt (field: Field) =
            { field with SelectionState = Some { selectionState with DraggingState = draggingStateOpt }}

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

            let selectionState =
                match field.SelectionState with
                | Some selectionState -> { selectionState with Location = coord }
                | None -> { Location = coord; DraggingState = None }
            { field with SelectionState = Some selectionState }
            |> updateCell (Cell.AddCounters counters) coord

        let clearCellsSelection coord field =
            unblockedDirsWithItself coord field
            |> Seq.iter (updateCellAt field (Cell.SetSelection NotSelected))
            field

    let init () =
        HexData.hexes
        |> Dictionary
        |> Helpers.create
        |> Helpers.setCounters (-4, 0) [| 5 |]
        |> Helpers.setCounters (-5, 0) [| 2 |]
        |> Helpers.setCounters (-4, -1) [| 1 |]
        |> Helpers.setCounters (-4, 1) [| 51 |], Cmd.none

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

    let update (msg: Msg) (state: Field) =
        match state.SelectionState, msg with
        | Some _, CellMsg(coord, Cell.CounterMsg(_, Counter.BeginDrag e)) ->
            state
            |> Helpers.updateCell (Cell.SetSelection CanBeDropped) coord
            |> Helpers.update, Cmd.OfAsync.perform doDrag e (fun _ -> EndDragging)
        | Some { DraggingState = Some draggingState } as Some selectionState,
          EndDragging ->
            Helpers.setDraggingState selectionState None state
            |> Helpers.clearCellsSelection draggingState.TargetCoord, Cmd.none
        | Some selectionState, CellMsg(coord, Cell.DragEntered)
          when not <| Option.exists (_.TargetCoord >> ((=) coord)) selectionState.DraggingState ->
            let draggingState =
                match selectionState.DraggingState with
                | Some draggingState ->
                    let cost =
                        match state[coord].Selection with
                        | MovedFrom -> (Terrain.cost state[coord].Terrain) >> ((-)0)
                        | _ -> Terrain.cost state[coord].Terrain
                    let updateMP = UnitCounter.payMP cost
                    let counters = Array.map (Counter.updateUnit updateMP) draggingState.Counters

                    Helpers.clearCellsSelection draggingState.TargetCoord state
                    |> Helpers.updateCell (Cell.SetSelection MovedFrom) draggingState.TargetCoord |> ignore

                    { draggingState with Counters = counters }
                | None -> { TargetCoord = coord; Counters = Cell.selectedCounters state[coord] }
            
            let units = Array.map (_.Info >> CounterInfo.unbox) draggingState.Counters
            Helpers.unblockedDirections coord state
            |> if draggingState.TargetCoord = coord
               then id
               else Seq.filter ((<>) draggingState.TargetCoord)
            |> Seq.filter (fun coord -> Array.forall (Terrain.canMoveTo state[coord].Terrain) units)
            |> Seq.iter (Helpers.updateCellAt state (Cell.SetSelection CanMoveTo))

            state
            |> Helpers.setDraggingState selectionState (Some { draggingState with TargetCoord = coord })
            |> Helpers.updateCell (Cell.SetSelection CanBeDropped) coord,
            Cmd.none
        | Some { Location = loc; DraggingState = Some draggingState },
          CellMsg(coord, Cell.Dropped) ->
            Helpers.removeCounters loc state
            |> Helpers.addCounters coord draggingState.Counters
            |> Helpers.update, Cmd.none
        | _, CellMsg(coord, cellMsg) ->
            let state' = Helpers.updateCell cellMsg coord state

            match cellMsg with
            | Cell.CounterMsg(_, Counter.ChangeSelection _)
            | Cell.AddCounters _
            | Cell.LiftCounter _ ->
                Option.map _.Location state.SelectionState
                |> Option.filter ((<>) coord)
                |> Option.iter (Helpers.updateCellAt state' Cell.DeselectCounters)

                let selectionStateOpt =
                    if Array.isEmpty state'[coord].Tower.SelectedIDs
                    then None
                    else Some { Location = coord; DraggingState = None }
                { state' with SelectionState = selectionStateOpt }
            | _ -> state'
            |> Helpers.update, Cmd.none

                
        


    let mapImage = Bitmap.create "avares://NoRetreat/Assets/Images/Map.jpg"

    let view (state: Field) (dispatch: Msg -> unit) : IView =
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
                                    match state.SelectionState with
                                    | None -> ()
                                    | Some { Location = coord } ->
                                        Canvas.onKeyDown (fun e->
                                            e.Handled <- true
                                            match e.Key with
                                            | Key.Up -> dispatchCell coord (Cell.LiftCounter true)
                                            | Key.Down -> dispatchCell coord (Cell.LiftCounter false)
                                            | _ -> ()
                                        )
                                    Canvas.children
                                        [ yield!
                                              state.Cells.Values
                                              |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coord))
                                          match state.SelectionState with
                                          | Some selectionState when selectionState.DraggingState.IsSome ->
                                              yield MovableBorder.create
                                                    [ MovableBorder.zIndex 10
                                                      MovableBorder.opacity 0.7
                                                      MovableBorder.child (
                                                          Cell.towerView
                                                              (Tower.selectedCounters state[selectionState.Location].Tower|> Tower.create)
                                                              ignore
                                                      ) ]
                                          | _ -> ()
                                        ]
                                  ]
                            ]
                      ]
              ) 
            ]
