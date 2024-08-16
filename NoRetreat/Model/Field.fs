namespace NoRetreat

#nowarn "25"

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
type SelectedCountersID =
    { Coord: Coordinate
      Indexes: int array }

module SelectedCountersID =
    let createOpt coord idxs =
        if Array.isEmpty idxs then
            None
        else
            Some { Coord = coord; Indexes = idxs }

type Field =
    { UpdateValue: bool
      Cells: Dictionary<Coordinate, Cell>
      SelectedLoc: SelectedCountersID option
      
      DraggingTo: Coordinate option
      }

    member this.Item
        with get coord = this.Cells[coord]
        and set coord cell = this.Cells[coord] <- cell

    member this.Update() =
        { this with
            UpdateValue = not this.UpdateValue }

module Field =
    module Helpers =
        let contains (field: Field) coord = field.Cells.ContainsKey coord

        let get (field: Field) coord = field[coord]

        let tryFind (field: Field) coord =
            match field.Cells.TryGetValue(coord) with
            | true, cell -> Some cell
            | false, _ -> None

        let unblockedDirections coord (field: Field) =
            Cell.Helpers.unblockedDirections field[coord] |> Seq.filter (contains field)

        let unblockedDirWithItself coord field =
            seq {
                yield! unblockedDirections coord field
                yield coord
            }

        let update (field: Field) = field.Update()

        let updateCell msg coord (field: Field) =
            let cell', extData = Cell.update msg field[coord]
            field[coord] <- cell'
            field, extData

        let updateCellAt (field: Field) msg coord =
            field[coord] <- Cell.update msg field[coord] |> fst

        let create dictionary =
            { UpdateValue = false
              Cells = dictionary
              SelectedLoc = None
              DraggingTo = None }

        let setCounters rawCoords ids (field: Field) =
            let coord = Coordinate.create rawCoords
            let counters = Array.map Counter.init ids
            let owner = CounterInfo.unbox counters[0].Counter |> _.Country

            field[coord] <-
                { field[coord] with
                    Tower = Tower.create counters }
            unblockedDirWithItself coord field 
            |> Seq.iter (updateCellAt field (Cell.AddZOC owner))
            field

        let removeCounters countersLoc (field: Field) =
            let coord = countersLoc.Coord
            let (Some owner) = field[coord].Tower.Owner
            unblockedDirWithItself coord field 
            |> Seq.iter (updateCellAt field (Cell.SubZOC owner))

            let (Cell.RemovedCounters counters) =
                updateCell (Cell.RemoveCounters countersLoc.Indexes) coord field |> snd

            field, counters

        let addCounters coord idxOpt (field: Field) (counters: Counter array) =
            let owner = (CounterInfo.unbox counters[0].Counter).Country
            unblockedDirWithItself coord field 
            |> Seq.iter (updateCellAt field (Cell.AddZOC owner))

            let (Cell.SelectedIdxs idxs) =
                updateCell (Cell.AddCounters(idxOpt, counters)) coord field |> snd

            { field with
                SelectedLoc = SelectedCountersID.createOpt coord idxs }

        let clearCellsSelection coord  field =
            unblockedDirections coord field
            |> Seq.iter (updateCellAt field (Cell.SetSelection NotSelected))
            field

        let selectedCounters (field: Field) =
            match field.SelectedLoc with
            | None -> Array.empty
            | Some selectedLoc ->
                let tower = field[selectedLoc.Coord].Tower
                Array.map (Tower.get tower) selectedLoc.Indexes

    let init () =
        HexData.hexes
        |> Dictionary
        |> Helpers.create
        |> Helpers.setCounters (-4, 0) [| 5 |]
        |> Helpers.setCounters (-5, 0) [| 2 |]
        |> Helpers.setCounters (-4, -1) [| 1 |]
        |> Helpers.setCounters (-4, 1) [| 51 |]

    type Msg = CellMsg of Coordinate * Cell.Msg

    let update (msg: Msg) (state: Field) =
        match msg with
        | CellMsg(coord, Cell.CounterMsg(_, Counter.BeginDrag e)) when Option.isSome state.SelectedLoc ->
            let data = DataObject()
            data.Set(DataFormats.Counters, 0)

            Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                DragDrop.DoDragDrop(e, data, DragDropEffects.Move))
            |> ignore

            state
        | CellMsg(coord, Cell.DragEntered) when Option.contains coord state.DraggingTo |> not ->
            match state.DraggingTo with
            | None -> ()
            | Some oldCoord ->
                Helpers.clearCellsSelection oldCoord state
                |> Helpers.updateCell (Cell.SetSelection MovedFrom) oldCoord |> ignore
            
            Helpers.unblockedDirections coord state
            |> Seq.filter (Helpers.get state >> _.Selection >> function
                | MovedFrom -> false
                | _ -> true)
            |> Seq.iter (Helpers.updateCellAt state (Cell.SetSelection CanMoveTo))

            { state with DraggingTo = Some coord }
            |> Helpers.updateCell (Cell.SetSelection NotSelected) coord
            |> fst
        | CellMsg(coord, Cell.Dropped) when Option.isSome state.SelectedLoc ->
            let (Some selectedLoc) = state.SelectedLoc

            { state with DraggingTo = None }
            |> Helpers.clearCellsSelection coord
            |> Helpers.removeCounters selectedLoc
            ||> Helpers.addCounters coord None
            |> Helpers.update
        | CellMsg(coord, cellMsg) ->
            let state', extData = Helpers.updateCell cellMsg coord state

            match extData with
            | Cell.SelectedIdxs idxs ->
                state'.SelectedLoc
                |> Option.map _.Coord
                |> Option.filter ((<>) coord)
                |> Option.iter (Helpers.updateCellAt state Cell.DeselectCounters)
            
                { state' with
                    SelectedLoc = SelectedCountersID.createOpt coord idxs }
            | _ -> Helpers.update state'


    let mapImage = Bitmap.create "avares://NoRetreat/Assets/Images/Map.jpg"

    let view (state: Field) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        ScrollPane.create
            [ ScrollPane.child (
                  Panel.create
                      [ Panel.height 3365
                        Panel.width 5750
                        Panel.children
                            [ 
                              Image.create
                                  [ Image.source mapImage
                                    Image.stretch Avalonia.Media.Stretch.None ]
                              Canvas.create
                                  [ Canvas.horizontalAlignment HorizontalAlignment.Stretch
                                    Canvas.verticalAlignment VerticalAlignment.Stretch
                                    Canvas.focusable true
                                    Canvas.onKeyDown ((fun e ->
                                        e.Handled <- true
                                        state.SelectedLoc
                                        |> Option.iter (fun { Coord = coord; Indexes = idxs } ->
                                            match e.Key with
                                            | Key.Up -> dispatchCell coord (Cell.LiftCounterUp idxs)
                                            | Key.Down -> dispatchCell coord (Cell.LiftCounterDown idxs)
                                            | _ -> ()
                                        )), SubPatchOptions.OnChangeOf state.SelectedLoc)
                                    Canvas.children
                                        [ yield!
                                              state.Cells.Values
                                              |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coord))
                                          if state.DraggingTo.IsSome then
                                              yield MovableBorder.create
                                                    [ MovableBorder.zIndex 10
                                                      MovableBorder.opacity 0.7
                                                      MovableBorder.child (
                                                          Cell.towerView
                                                              (Helpers.selectedCounters state |> Tower.create)
                                                              ignore
                                                      ) ] 
                                        ] ] ] ]
              ) ]
