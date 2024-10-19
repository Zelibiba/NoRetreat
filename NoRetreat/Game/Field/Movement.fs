module NoRetreat.Game.Field.Movement
open NoRetreat
open NoRetreat.Game
open Helpers

let removeCounters coord (field: T) =
    let counters = Cell.selectedCounters field[coord]
    if counters.Length = 0 then
        field
    else
        updateTowerAt (Tower.RemoveCounters counters) coord field
        |> subZOC counters coord

let addCounters coord counters (field: T) =
    updateTowerAt (Tower.AddCounters counters) coord field
    |> addZOC counters coord

let clearCellsSelection coord field =
    unblockedCellsWithItself coord field
    |> updateCells (Cell.SetSelection Cell.NotSelected) field

let defineMovements coord counters (field: T) =
    let adjacentCells = unblockedCells coord field
    
    adjacentCells
    |> Array.map (fun cell ->
        Array.map (Cell.getMovementSelection cell) counters
        |> Array.reduce (fun selection1 selection2 -> 
            match selection1, selection2 with
            | Cell.Selection.NotSelected, _
            | _, Cell.NotSelected -> Cell.Selection.NotSelected
            | Cell.MovedFrom, Cell.MovedFrom -> Cell.MovedFrom
            | _, _ -> Cell.CanMoveTo))
    |> updateCells' Cell.SetSelection field adjacentCells
    |> updateCellAt (Cell.SetSelection Cell.CanBeDropped) coord
