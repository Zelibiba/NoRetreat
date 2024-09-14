module NoRetreat.Game.Field.Supply
open NoRetreat
open NoRetreat.Game
open Helpers

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
    let supplyCoordsUSSR =
        [ (0, 1); (1, 0); (-1, 1) ]
        |> List.map Coordinate.create
        |> List.toSeq

    let supplyCoordsGermany =
        [ (0, -1); (1, -1); (-1, 0) ]
        |> List.map Coordinate.create
        |> List.toSeq

    let can'tBePath country (cell: Cell.T) =
        Cell.ZOC.isEZOCFor country cell.ZOC && not <| cell.BelongsTo country ||
        match cell.Terrain with
        | Cell.City city -> city.Owner <> country
        | Cell.Area -> true
        | _ -> false

    let isCitySupplied coord =
        let rec isSupplied 
            (visitedCoords: Dictionary<Coordinate, bool>)
            (field: T)
            country 
            (neighbours: Coordinate -> Coordinate seq)
            coord =
            match visitedCoords.TryGetValue(coord) with
            | true, value -> value
            | false, _ ->
                let cell = field[coord]

                if can'tBePath country cell then
                    visitedCoords[coord] <- false
                    false
                else if Option.contains country cell.MapEdge then
                    visitedCoords[coord] <- true
                    true
                else
                    visitedCoords[coord] <- false

                    let result =
                        neighbours coord
                        |> Seq.exists (isSupplied visitedCoords field country neighbours)
                    visitedCoords[coord] <- result
                    result

        fun (field, visited, country) ->
            let neighbours =
                match country with
                | USSR -> fun coord -> Seq.map ((+) coord) supplyCoordsUSSR
                | Germany -> fun coord -> Seq.map ((+) coord) supplyCoordsGermany
                >> Seq.filter (contains field)

            neighbours coord
            |> Seq.exists (isSupplied visited field country neighbours)
        |> SupplyAction

    let directSuppliedCells pathLength coords =
        let suppliedPathes
            (visitedCoords: Dictionary<Coordinate, bool>)
            (field: T) country coord =
            if visitedCoords.ContainsKey(coord) then
                Array.empty
            else if can'tBePath country field[coord] then
                visitedCoords[coord] <- false
                Array.empty
            else
                visitedCoords[coord] <- true
                let arr =
                    Helpers.adjacentCoords coord
                    |> Seq.choose (tryFind field)
                    |> Seq.filter (_.MapEdge >> Option.contains country >> not)
                    |> Seq.map _.Coord
                    |> Seq.toArray
                arr
        fun (field, visited, country) ->
            Array.fold (fun state _ ->
                Array.collect (suppliedPathes visited field country) state)
                coords [| 0..pathLength |] |> ignore

            let enemyCountry = country.Opposite
        
            visited.Keys
            |> Seq.filter (field.get >> Cell.belongsTo enemyCountry >> not)
        |> SupplyAction

    let defineCitiesSupply (country, field) cities =
        let supplies =
            SupplyAction.traverse isCitySupplied cities
            |> SupplyAction.execute country field
            |> Seq.map (fun isSupplied -> (country, isSupplied))

        Seq.iter2 (fun coord supply ->
            Helpers.updateCellAt field (Cell.SetSupply supply) coord)
            cities supplies

        (country, field)

    let defineAllCitiesSupply (country, field) =
        defineCitiesSupply (country, field) field.CityCoords[country]

    let defineCellsSupply (country, field) =
        let suppliedFromCities =
            field.CityCoords[country]
            |> List.toArray
            |> Array.filter (field.get >> _.Supply[country])
            |> directSuppliedCells 4
        
        let suppliedFromEdge =
            field.MapEdgeCoords[country]
            |> directSuppliedCells 3
        
        let (<*>) = SupplyAction.apply
        let suppliedCellsCoord =
            SupplyAction.rtrn Seq.append <*> suppliedFromEdge <*> suppliedFromCities
            |> SupplyAction.map Seq.distinct
            |> SupplyAction.execute country field
    
        field.Cells.Keys
        |> Seq.iter (updateCellAt field (Cell.SetSupply (country, false)))

        Seq.iter (updateCellAt field (Cell.SetSupply (country, true))) suppliedCellsCoord

    let checkAlterSupply (field: T) country coord =
        match field[coord].Sea with
        | Some Cell.Baltic
        | Some Cell.Adriatic -> country = Germany
        | Some Cell.Caspian -> country = USSR
        | Some Cell.Black -> true
        | None ->
            Helpers.adjacentCoords coord
            |> Seq.choose (Helpers.tryFind field)
            |> Seq.filter (Cell.belongsTo country)
            |> Seq.exists _.Supply[country]

open Helpers

let calculateDirectSupply field country =
    defineAllCitiesSupply (country, field)
    |> defineCellsSupply

let defineUnitsSupply (field: T) country =
    field.Cells.Values
    |> Seq.filter (Cell.belongsTo country)
    |> Seq.tuple (fun cell -> cell.Supply[country] || (checkAlterSupply field country cell.Coord))
    |> Seq.iter (fun (cell, isSupplied) ->
        Helpers.updateTowerAt field (Tower.UpdateAllCounters <| Counter.SetSupply isSupplied) cell.Coord)