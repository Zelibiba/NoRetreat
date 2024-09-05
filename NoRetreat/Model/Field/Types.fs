[<AutoOpen>]
module NoRetreat.Field.Types

open NoRetreat
open System.Collections.Generic

[<Struct>]
type SelectionState =
    | NotSelected
    | Selected of coord: Coordinate
    | Dragging of origCoord: Coordinate * currentCoord: Coordinate * counters: Counter array

type Field =
    { UpdateValue: bool
      Cells: Dictionary<Coordinate, Cell> 
      CityCoords: Dictionary<Country, Coordinate list>
      MapEdgeCoords: Dictionary<Country, Coordinate array> }

    member x.Item
        with get coord = x.Cells[coord]
        and set coord cell = x.Cells[coord] <- cell

    member x.get coord = x.Cells[coord]