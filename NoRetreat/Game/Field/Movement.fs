module NoRetreat.Game.Field.Movement
open NoRetreat
open NoRetreat.Game
open Helpers

let removeCounters coord (field: T) =
    let counters = Cell.selectedCounters field[coord]
    if counters.Length = 0 then
        field
    else
        updateTower Tower.RemoveCounters coord field
        |> subZOC counters.Length counters[0].Country coord

let addCounters coord counters (field: T) =
    updateTower (Tower.AddCounters counters) coord field
    |> addZOC counters.Length counters[0].Country coord

let clearCellsSelection coord field =
    unblockedCellsWithItself coord field |> Array.map _.Coord
    |> updateCells (Cell.SetSelection Cell.NotSelected) field

let defineMovements coord counters (field: T) =
    let adjacentCells = unblockedCells coord field
    let adjacentCoords = adjacentCells |> Array.map _.Coord
    
    adjacentCells
    |> Array.map (fun cell ->
        Array.map (Cell.getMovementSelection cell) counters
        |> Array.reduce (fun selection1 selection2 -> 
            match selection1, selection2 with
            | Cell.Selection.NotSelected, _
            | _, Cell.NotSelected -> Cell.Selection.NotSelected
            | Cell.MovedFrom, Cell.MovedFrom -> Cell.MovedFrom
            | _, _ -> Cell.CanMoveTo))
    |> Array.foldBack2 (Cell.SetSelection >> updateCell) <| adjacentCoords <| field
    |> updateCell (Cell.SetSelection Cell.CanBeDropped) coord
