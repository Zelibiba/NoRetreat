namespace NoRetreat

#nowarn "25"

open System.Collections.Generic
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.Input
open Avalonia.Media.Imaging
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Elmish
open NoRetreat.Controls
open HexGameControls


[<Struct>]
type SelectedCountersID =
    { Coords: Coordinates
      Indexes: int array }

module SelectedCountersID =
    let createOpt coords idxs =
        if Array.isEmpty idxs then
            None
        else
            Some { Coords = coords; Indexes = idxs }

type Field =
    { UpdateValue: bool
      Cells: Dictionary<Coordinates, Cell>
      SelectedLoc: SelectedCountersID option }

    member this.Item
        with get coords = this.Cells[coords]
        and set coords cell = this.Cells[coords] <- cell

    member this.Update() =
        { this with
            UpdateValue = not this.UpdateValue }

module Field =
    module Helpers =
        let update (field: Field) = field.Update()

        let updateCell msg coords (field: Field) =
            let cell', extData = Cell.update msg field[coords]
            field[coords] <- cell'
            field, extData

    let create dictionary =
        { UpdateValue = false
          Cells = dictionary
          SelectedLoc = None }

    let init () =
        HexData.hexes
        |> Dictionary
        |> create

    type Msg = CellMsg of Coordinates * Cell.Msg

    let update (msg: Msg) (state: Field) =
        match msg with
        | CellMsg(_, Cell.CounterMsg(_, Counter.BeginDrag e)) ->
            if Option.isSome state.SelectedLoc then
                let data = DataObject()
                data.Set(DataFormats.Counters, 0)

                Dispatcher.UIThread.InvokeAsync<DragDropEffects>(fun _ ->
                    DragDrop.DoDragDrop(e, data, DragDropEffects.Move))
                |> ignore

            state
        | CellMsg(coords, Cell.CounterMsg(idx, Counter.Dropped)) when Option.isSome state.SelectedLoc ->
            let (Some selectedLoc) = state.SelectedLoc

            let idx' =
                if selectedLoc.Coords = coords then
                    selectedLoc.Indexes |> Array.filter ((>) idx) |> Array.length |> (-) idx
                else
                    idx

            let state', (Cell.RemovedCounters counters) =
                Helpers.updateCell (Cell.RemoveCounters selectedLoc.Indexes) selectedLoc.Coords state

            let state'', (Cell.SelectedIdxs idxs) =
                Helpers.updateCell (Cell.AddCounters(Some idx', counters)) coords state'

            { state'' with
                SelectedLoc = SelectedCountersID.createOpt coords idxs }
            |> Helpers.update
        | CellMsg(coords, Cell.Dropped) when Option.isSome state.SelectedLoc ->
            let (Some selectedLoc) = state.SelectedLoc

            let state', (Cell.RemovedCounters counters) =
                Helpers.updateCell (Cell.RemoveCounters selectedLoc.Indexes) selectedLoc.Coords state

            let state'', (Cell.SelectedIdxs idxs) =
                Helpers.updateCell (Cell.AddCounters(None, counters)) coords state'

            { state'' with
                SelectedLoc = SelectedCountersID.createOpt coords idxs }
            |> Helpers.update
        | CellMsg(coords, cellMsg) ->
            let state', extData = Helpers.updateCell cellMsg coords state

            match extData with
            | Cell.SelectedIdxs idxs ->
                match state'.SelectedLoc with
                | Some { Coords = coords' } when coords' <> coords ->
                    state'[coords'] <- Cell.update Cell.DeselectCounters state'[coords'] |> fst
                | _ -> ()

                { state' with
                    SelectedLoc = SelectedCountersID.createOpt coords idxs }
            | _ -> state'
            |> Helpers.update


    let view (state: Field) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        ScrollPane.create [
            
            ScrollPane.child (
                Panel.create [
                    Panel.height 3365
                    Panel.width 5750
                    Panel.children [
                        Image.create [ 
                            Image.source  ("avares://NoRetreat/Assets/Images/Map.jpg" |> Bitmap.create)
                            Image.stretch Avalonia.Media.Stretch.None
                        ]
                        Canvas.create [
                          Canvas.horizontalAlignment HorizontalAlignment.Stretch
                          Canvas.verticalAlignment VerticalAlignment.Stretch
                          Canvas.children (
                              state.Cells.Values
                              |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coords))
                              |> Seq.toList
                          )
                        ]
                        
                    ]
                ]
            )
        ]

        
