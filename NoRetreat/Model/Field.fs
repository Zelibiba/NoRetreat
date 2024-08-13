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
      IsDragged: bool }

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

        let create dictionary =
            { UpdateValue = false
              Cells = dictionary
              SelectedLoc = None
              IsDragged = false }

        let setCounters rawCoords ids (field: Field) =
            let coord = Coordinate.create rawCoords
            let counters = Array.map Counter.init ids
            let (Unit unit) = counters[0].Counter

            field[coord] <-
                { field[coord] with
                    Tower = Tower.create counters
                    Owner = Some unit.Country }

            field

        let defineZOC (field: Field) coord =
            let zoc' =
                unblockedDirWithItself coord field
                |> Seq.map (get field >> _.Owner)
                |> Seq.fold ZOC.add NoZOC

            updateCell (Cell.SetZOC zoc') coord field |> fst

        let initZOC (field: Field) =
            Seq.iter (defineZOC field >> ignore) field.Cells.Keys
            field

        let removeCounters countersLoc (field: Field) =
            let coord = countersLoc.Coord

            let (Cell.RemovedCounters counters) =
                updateCell (Cell.RemoveCounters countersLoc.Indexes) coord field |> snd

            if field[coord].Tower.Counters.Length = 0 then
                unblockedDirWithItself coord field |> Seq.iter (defineZOC field >> ignore)

            field, counters

        let addCounters coord idxOpt (field: Field) counters =
            let number = field[coord].Tower.Counters.Length

            let (Cell.SelectedIdxs idxs) =
                updateCell (Cell.AddCounters(idxOpt, counters)) coord field |> snd

            if number = 0 then
                unblockedDirWithItself coord field |> Seq.iter (defineZOC field >> ignore)

            { field with
                SelectedLoc = SelectedCountersID.createOpt coord idxs }

        let selectedCounters (field: Field) =
            match field.SelectedLoc with
            | None -> Array.empty
            | Some selectedLoc ->
                let tower = field[selectedLoc.Coord].Tower
                Array.map (Tower.get tower) selectedLoc.Indexes

    let init () =
        HexData.hexes
        //|> Seq.skip 70
        //|> Seq.take 40
        |> Dictionary
        |> Helpers.create
        |> Helpers.setCounters (-4, 0) [| 5 |]
        |> Helpers.setCounters (-5, 0) [| 2 |]
        |> Helpers.setCounters (-4, -1) [| 1 |]
        |> Helpers.setCounters (-4, 1) [| 51 |]
        |> Helpers.initZOC

    type Msg = CellMsg of Coordinate * Cell.Msg

    let update (msg: Msg) (state: Field) =
        match msg with
        | CellMsg(_, Cell.CounterMsg(_, Counter.BeginDrag e)) ->
            if Option.isSome state.SelectedLoc then
                let data = DataObject()
                data.Set(DataFormats.Counters, 0)

                Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                    DragDrop.DoDragDrop(e, data, DragDropEffects.Move))
                |> ignore

            { state with IsDragged = true }
        | CellMsg(coord, Cell.CounterMsg(idx, Counter.Dropped)) when Option.isSome state.SelectedLoc ->
            let (Some selectedLoc) = state.SelectedLoc

            let idx' =
                if selectedLoc.Coord = coord then
                    selectedLoc.Indexes |> Array.filter ((>) idx) |> Array.length |> (-) idx
                else
                    idx

            { state with IsDragged = false }
            |> Helpers.removeCounters selectedLoc
            ||> Helpers.addCounters coord (Some idx')
            |> Helpers.update
        | CellMsg(coord, Cell.Dropped) when Option.isSome state.SelectedLoc ->
            let (Some selectedLoc) = state.SelectedLoc

            { state with IsDragged = false }
            |> Helpers.removeCounters selectedLoc
            ||> Helpers.addCounters coord None
            |> Helpers.update
        | CellMsg(coord, cellMsg) ->
            let state', extData = Helpers.updateCell cellMsg coord state

            match extData with
            | Cell.SelectedIdxs idxs ->
                match state'.SelectedLoc with
                | Some { Coord = coord' } when coord' <> coord ->
                    state'[coord'] <- Cell.update Cell.DeselectCounters state'[coord'] |> fst
                | _ -> ()

                { state' with
                    SelectedLoc = SelectedCountersID.createOpt coord idxs }
            | _ -> state'
            |> Helpers.update


    let view (state: Field) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        ScrollPane.create
            [ ScrollPane.child (
                  Panel.create
                      [ Panel.height 3365
                        Panel.width 5750
                        Panel.children
                            [ Image.create
                                  [ Image.source ("avares://NoRetreat/Assets/Images/Map.jpg" |> Bitmap.create)
                                    Image.stretch Avalonia.Media.Stretch.None ]
                              //ItemsControl.create [
                              //    ItemsControl.horizontalAlignment HorizontalAlignment.Stretch
                              //    ItemsControl.verticalAlignment VerticalAlignment.Stretch
                              //    ItemsControl.itemsPanel (
                              //        {
                              //            new ITemplate<Panel> with
                              //                member this.Build() : Panel =
                              //                    let c = Canvas()
                              //                    c :> _
                              //
                              //            interface Avalonia.Styling.ITemplate with
                              //                member this.Build() : obj =
                              //                    let c = Canvas()
                              //                    c :> _
                              //        }
                              //    )
                              //    ItemsControl.itemTemplate (
                              //        DataTemplateView<Cell>.create (fun cell -> Cell.view cell (dispatchCell cell.Coord))
                              //    )
                              //    ItemsControl.dataItems state.Cells.Values
                              //]
                              Canvas.create
                                  [ Canvas.name "Canvas"
                                    Canvas.horizontalAlignment HorizontalAlignment.Stretch
                                    Canvas.verticalAlignment VerticalAlignment.Stretch
                                    Canvas.children
                                        [ yield!
                                              state.Cells.Values
                                              |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coord))
                                          if state.IsDragged then
                                              yield
                                                  MovableBorder.create
                                                      [ MovableBorder.zIndex 10
                                                        MovableBorder.opacity 0.7
                                                        MovableBorder.child (
                                                            Cell.towerView
                                                                (Helpers.selectedCounters state |> Tower.create)
                                                                ignore
                                                        ) ] ] ] ] ]
              ) ]
