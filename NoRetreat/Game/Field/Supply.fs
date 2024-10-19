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
        let private supplyCoordsGermany   = Array.map Coordinate.create [| (0, -1); (1, -1); (-1, 0) |]
        let private supplyCoordsUSSR      = Array.map Coordinate.create [| (0, 1); (1, 0); (-1, 1) |]
        let private supplyCoordsLeningrad = Array.map Coordinate.create [| (-8, 11); (-8, 12) |]
        
        let For country (field: T) =
            match country with
            | USSR -> fun coord ->
                if (coord.R, coord.C) = (-9, 11)
                then supplyCoordsLeningrad
                else Array.map ((+) coord) supplyCoordsUSSR
            | Germany -> fun coord -> Array.map ((+) coord) supplyCoordsGermany
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
            //|> Array.filter visitedCoords.get_Item
            |> Array.filter (field.getCell >> Cell.belongsTo enemyCountry >> not)

    let defineCitiesSupply country field citiesCoords =
        SupplyAction.traverse isCitySupplied citiesCoords
        |> SupplyAction.execute country field
        |> List.toArray
        |> Array.map (fun isSupplied -> (country, isSupplied))
        |> Helpers.updateCellsAt' Cell.SetSupply field (List.toArray citiesCoords)

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
        let suppliedCellsCoords = 
            SupplyAction.rtrn Array.append <*> suppliedFromEdge <*> suppliedFromCities
            |> SupplyAction.execute country field

        let allCoords = 
            field.Cells |> Seq.toArray
            |> Array.map _.Coord
            |> Set.ofArray
        let unsuppliedCellsCoords = 
            Set.difference allCoords <| Set.ofArray suppliedCellsCoords
            |> Set.toArray

        let field' = Helpers.updateCellsAt (Cell.SetSupply (country, false)) field unsuppliedCellsCoords
        Helpers.updateCellsAt (Cell.SetSupply (country, true)) field' suppliedCellsCoords

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

let defineUnitsSupply country (field: T) =
    let countersWithZOC (cell: Cell.T) = 
        let _, quantity = List.toArray cell.Tower.Counters |> Counter.getZOCModification
        quantity

    field.Cells |> Seq.toArray
    |> Array.filter (Cell.belongsTo country)
    |> Array.fold (fun (field: T) cell ->
        let isSupplied = cell.Supply[country] || (checkAlterSupply country field cell.Coord)
        let cell' = Cell.updateTower (Tower.UpdateAllCounters <| Counter.SetSupply isSupplied) cell
        let field' = field.updateCell (fun _ -> cell') cell.Coord

        let quantity = countersWithZOC cell
        let quantity' = countersWithZOC cell'
        let diff = quantity' - quantity
        if diff <> 0 
            then Helpers.changeZOC (country, diff) cell.Coord field'
            else field'
        ) field

let checkSupply field country =
    calculateDirectSupply field country
    |> defineUnitsSupply country