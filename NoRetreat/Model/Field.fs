namespace NoRetreat

open System.Collections.Generic
open System.Collections.Immutable
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Elmish
open NoRetreat.Controls
open HexGameControls

type SelectedCountersID =
    { Coords: Coordinates
      Indexes: int array }

module SelectedCounterLocations =
    let createOpt coords idxs =
        if Array.isEmpty idxs then None
        else Some { Coords = coords; Indexes = idxs }

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

    let create dictionary =
        { UpdateValue = false
          Cells = dictionary
          SelectedLoc = None }

    let update' (field: Field) = field.Update()

    let init () =
        let coordinates = [ (0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (2, 0) ] |> List.map Coordinates.create
        let countersVal = [      0;      4;      4;      4;      0;      0 ]

        List.map2 (fun coords cVal -> KeyValuePair.Create(coords, Cell.init (coords, cVal))) coordinates countersVal
        |> Dictionary
        |> create

    type Msg = CellMsg of Coordinates * Cell.Msg

    let update (msg: Msg) (state: Field) =
        match msg with
        | CellMsg(coords, cellMsg) ->
            let cell', selectedIdxs = Cell.update cellMsg state[coords]
            state[coords] <- cell'
            
            match selectedIdxs with
            | Cell.Selected idxs ->
                match state.SelectedLoc with
                | Some { Coords = coords' } when coords' <> coords -> 
                    state[coords'] <- Cell.update Cell.DeselectCounters state[coords'] |> fst
                | _ -> ()

                { state with SelectedLoc = SelectedCounterLocations.createOpt coords idxs }
            | _ -> state
            |> update'


    let view (state: Field) (dispatch: Msg -> unit) : IView =
        let dispatchCell = Library.dispatchwithIndex dispatch CellMsg

        Canvas.create
            [ Canvas.background "gold"
              Canvas.margin (100, 100, 0, 0)
              Canvas.horizontalAlignment HorizontalAlignment.Stretch
              Canvas.verticalAlignment VerticalAlignment.Stretch
              Canvas.children (
                  state.Cells.Values
                  |> Seq.map (fun cell -> Cell.view cell (dispatchCell cell.Coords))
                  |> Seq.toList
              ) ]
