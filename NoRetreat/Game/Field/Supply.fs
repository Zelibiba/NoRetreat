module NoRetreat.Game.Field.Supply
open NoRetreat
open NoRetreat.Game

open System.Collections.Generic


type private SupplyAction<'a> = SupplyAction of (T * Dictionary<Coordinate, bool> * Country -> 'a)
module private SupplyAction =
    let run state (SupplyAction action) =
        action state

    let execute country field action =
        let visitedCoords = Dictionary<Coordinate, bool>()
        run (field, visitedCoords, country) action

    let rtrn x =
        SupplyAction (fun _ -> x)

    let map f action =
        fun state -> run state action |> f
        |> SupplyAction

    let apply fAction xAction =
        fun state ->
            let f = run state fAction 
            let x = run state xAction
            f x
        |> SupplyAction

    let bind f xAction =
        fun state ->
            (run state xAction) |> f |> run state
        |> SupplyAction

    let traverse f list =
        let (<*>) = apply
        let cat head tail = head :: tail
        let init = rtrn []
        let folder head tail = rtrn cat <*> f head <*> tail

        List.foldBack folder list init

module private Helpers =
    module private AdjacentCells =
        let private supplyCoordsUSSR = Array.map Coordinate.create [| (0, 1); (1, 0); (-1, 1) |]
        let private supplyCoordsGermany = Array.map Coordinate.create [| (0, -1); (1, -1); (-1, 0) |]
        
        let For country (field: T) =
            match country with
            | USSR -> supplyCoordsUSSR
            | Germany -> supplyCoordsGermany
            |> fun patternCoords coord -> Array.map ((+) coord) patternCoords
            |> field.choose

    let can'tBePathFor country (cell: Cell.T) =
        Cell.ZOC.isEZOCFor country cell.ZOC && not <| cell.belongsTo country ||
        match cell.Terrain with
        | Cell.Terrain.City city -> city.Owner <> country
        | Cell.Terrain.Area -> true
        | _ -> false

    let isCitySupplied cityCoord =
        SupplyAction <| fun (field, visitedCoords: Dictionary<Coordinate, bool>, country) ->
            let adjacentCells = AdjacentCells.For country field

            let rec isCellCorrect (cell: Cell.T) =
                let coord = cell.Coord
                match visitedCoords.TryGetValue(coord) with
                | true, value -> value
                | false, _ ->
                    if can'tBePathFor country cell then
                        visitedCoords[coord] <- false
                    else if Option.contains country cell.MapEdge then
                        visitedCoords[coord] <- true
                    else
                        visitedCoords[coord] <- adjacentCells coord |> Seq.exists isCellCorrect
                
                    visitedCoords[coord]

            adjacentCells cityCoord
            |> Seq.exists isCellCorrect

    let directSuppliedCells pathLength sourceCoords =
        SupplyAction <| fun (field, visitedCoords, country) ->
            let suppliedPathes (cell: Cell.T) =
                let coord = cell.Coord
                if can'tBePathFor country cell then
                    visitedCoords[coord] <- false
                    Array.empty
                else
                    visitedCoords[coord] <- true

                    Helpers.adjacentCells coord field
                    |> Array.filter (_.Coord >> visitedCoords.ContainsKey >> not)

            Array.iter (fun coord -> visitedCoords[coord] <- true) sourceCoords
            let sourceCells = Array.map field.getCell sourceCoords

            Array.fold (fun state _ -> Array.collect suppliedPathes state) sourceCells [| 0..pathLength |] |> ignore
            
            let enemyCountry = country.Opposite
            visitedCoords.Keys |> Seq.toArray
            |> Array.filter visitedCoords.get_Item
            |> Array.filter (field.getCell >> Cell.belongsTo enemyCountry >> not)

    let defineCitiesSupply country field citiesCoords =
        let supplies =
            SupplyAction.traverse isCitySupplied citiesCoords
            |> SupplyAction.execute country field
            |> List.map (fun isSupplied -> (country, isSupplied))

        List.foldBack2 (Cell.SetSupply >> Helpers.updateCell) supplies citiesCoords field

    let defineAllCitiesSupply country field = defineCitiesSupply country field field.CitiesCoords

    let defineCellsSupply country (field: T) =
        let suppliedFromCities =
            field.CitiesCoordsOf country
            |> List.toArray
            |> Array.filter (field.getCell >> _.Supply[country])
            |> directSuppliedCells 4
        
        let suppliedFromEdge =
            field.MapEdgesCoords[country]
            |> List.toArray
            |> directSuppliedCells 3
        
        let (<*>) = SupplyAction.apply
        let suppliedCellsCoord =
            SupplyAction.rtrn Array.append <*> suppliedFromEdge <*> suppliedFromCities
            |> SupplyAction.execute country field

        let field' =
            field.Cells |> Seq.toArray
            |> Array.map _.Coord
            |> Helpers.updateCells (Cell.SetSupply (country, false)) field

        Helpers.updateCells (Cell.SetSupply (country, true)) field' suppliedCellsCoord

    let checkAlterSupply country (field: T) coord =
        match field[coord].Sea with
        | Some Cell.Baltic
        | Some Cell.Adriatic -> country = Germany
        | Some Cell.Caspian -> country = USSR
        | Some Cell.Black -> true
        | None ->
            Helpers.adjacentCells coord field
            |> Seq.filter (Cell.belongsTo country)
            |> Seq.exists _.Supply[country]

open Helpers

let calculateDirectSupply field country =
    defineAllCitiesSupply country field
    |> defineCellsSupply country

let defineUnitsSupply (field: T) country =
    let cellsWithUnits = 
        field.Cells |> Seq.toArray
        |> Array.filter (Cell.belongsTo country)
    let cellsCoords = cellsWithUnits |> Array.map _.Coord

    cellsWithUnits
    |> Array.map (fun cell -> cell.Supply[country] || (checkAlterSupply country field cell.Coord))
    |> Array.foldBack2 (Counter.SetSupply >> Tower.UpdateAllCounters >> Helpers.updateTower) <| cellsCoords <| field