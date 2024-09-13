module NoRetreat.Game.Field.Movement
open NoRetreat
open NoRetreat.Game
open Helpers

let removeCounters coord (field: T) =
    let counters = Tower.selectedCounters field[coord].Tower
    if counters.Length = 0 then
        field
    else
        let owner = counters[0].Country
        unblockedDirsWithItself coord field
        |> Seq.iter (updateCellAt field <| Cell.SubZOC (owner, counters.Length))

        updateCell (Cell.TowerMsg Tower.RemoveCounters) coord field

let addCounters coord (counters: Counter.T array) (field: T) =
    let owner = counters[0].Country
    unblockedDirsWithItself coord field
    |> Seq.iter (updateCellAt field <| Cell.AddZOC (owner, counters.Length))

    updateCell (Cell.TowerMsg <| Tower.AddCounters counters) coord field

let clearCellsSelection coord field =
    unblockedDirsWithItself coord field
    |> Seq.iter (updateCellAt field (Cell.SetSelection Cell.NotSelected))

    field

let defineMovements coord counters (field: T) =
    unblockedDirections coord field
    |> Seq.tuple (field.get 
        >> (fun cell -> Array.map (Cell.getMovementSelection cell) counters)
        >> Array.reduce (fun selection1 selection2 -> 
            match selection1, selection2 with
            | Cell.Selection.NotSelected, _
            | _, Cell.NotSelected -> Cell.Selection.NotSelected
            | Cell.MovedFrom, Cell.MovedFrom -> Cell.MovedFrom
            | _, _ -> Cell.CanMoveTo))
    |> Seq.iter (fun (coord, selection) -> 
        updateCellAt field (Cell.SetSelection selection) coord)

    field