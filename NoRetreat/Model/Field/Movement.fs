module NoRetreat.Field.Movement


open NoRetreat
open NoRetreat.Field.Helpers

let removeCounters coord (field: Field) =
    let counters = Tower.selectedCounters field[coord].Tower
    if counters.Length = 0 then
        field
    else
        let owner = counters[0].Country
        unblockedDirsWithItself coord field
        |> Seq.iter (updateCellAt field <| Cell.SubZOC (owner, counters.Length))

        updateCell (Cell.RemoveCounters) coord field

let addCounters coord (counters: Counter array) (field: Field) =
    let owner = counters[0].Country
    unblockedDirsWithItself coord field
    |> Seq.iter (updateCellAt field <| Cell.AddZOC (owner, counters.Length))

    updateCell (Cell.AddCounters counters) coord field

let clearCellsSelection coord field =
    unblockedDirsWithItself coord field
    |> Seq.iter (updateCellAt field (Cell.SetSelection CellSelection.NotSelected))
    field

let defineMovements coord counters (field: Field) =
    unblockedDirections coord field
    |> Seq.tuple (get field >> (fun cell ->
        Array.map (Cell.canMoveTo cell) counters)
        >> Array.reduce (fun selection1 selection2 -> 
            match selection1, selection2 with
            | CellSelection.NotSelected, _
            | _, CellSelection.NotSelected -> CellSelection.NotSelected
            | MovedFrom, MovedFrom -> MovedFrom
            | _, _ -> CanMoveTo))
    |> Seq.iter (fun (coord, selection) -> 
        updateCellAt field (Cell.SetSelection selection) coord)
    field