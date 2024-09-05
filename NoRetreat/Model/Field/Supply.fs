module NoRetreat.Field.Supply
open System.Collections.Generic
open NoRetreat


type private SupplyAction<'a> = SupplyAction of (Field * Dictionary<Coordinate, bool> * Country -> 'a)
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


let private supplyCoordsUSSR =
    [ (0, 1); (1, 0); (-1, 1) ]
    |> List.map Coordinate.create
    |> List.toSeq

let private supplyCoordsGermany =
    [ (0, -1); (1, -1); (-1, 0) ]
    |> List.map Coordinate.create
    |> List.toSeq

let private can'tBePath country (cell: Cell) =
    ZOC.isEZOCFor country cell.ZOC && not <| Option.contains country cell.Owner ||
    match cell.Terrain with
    | City city -> city.Owner <> country
    | _ -> false

let private isCitySupplied coord =
    let rec isSupplied 
        (visitedCoords: Dictionary<Coordinate, bool>)
        (field: Field)
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
            >> Seq.filter (Helpers.contains field)
        neighbours coord
        |> Seq.exists (isSupplied visited field country neighbours)
    |> SupplyAction

let private directSuppliedCells cellFilter pathLength coords =
    let suppliedPathes
        (visitedCoords: Dictionary<Coordinate, bool>)
        (field: Field) country coord =
        if visitedCoords.ContainsKey(coord) then
            Array.empty
        else if can'tBePath country field[coord] then
            visitedCoords[coord] <- false
            Array.empty
        else
            visitedCoords[coord] <- true
            Coordinate.adjacentCoords coord
            |> Seq.choose (Helpers.tryFind field)
            |> Seq.filter (_.MapEdge >> Option.contains country >> not)
            |> Seq.map _.Coord
            |> Seq.toArray
    fun (field, visited, country) ->
        Array.fold (fun state _ ->
            Array.collect (suppliedPathes visited field country) state)
            coords [| 0..pathLength |] |> ignore
        
        visited.Keys
        |> Seq.filter (fun coord -> visited[coord])
        |> Seq.filter (Helpers.get field >> cellFilter)
    |> SupplyAction

let defineCitiesSupply country field cities =
    let supplies =
        SupplyAction.traverse isCitySupplied cities
        |> SupplyAction.execute country field

    Seq.iter2 (fun coord isSupplied ->
        Helpers.updateCellAt field (Cell.SetCitySupply isSupplied) coord)
        cities supplies

    field

let defineAllCitiesSupply country field =
    defineCitiesSupply country field field.CityCoords[country]

//let isCellDirectSupplied country field coord =
//    directSuppliedCells (_.Terrain >> function
//        | City city -> city.Owner = country
//        | _ -> false) [coord]
//    |> SupplyAction.execute country field
//    |> Seq.toList
//    |> SupplyAction.traverse isCitySupplied
//    |> SupplyAction.execute country field
//    |> List.contains true

let getDiretSuppliedUnitsCoord country field =
    let suppliedFromCities =
        field.CityCoords[country]
        |> List.toArray
        |> Array.filter (Helpers.get field >> _.Terrain >> Terrain.getCity >> _.IsSupplied)
        |> directSuppliedCells (fun _ -> true) 4
        |> SupplyAction.execute country field

    let suppliedFromEdge =
        field.MapEdgeCoords[country]
        |> directSuppliedCells (fun _ -> true) 3 //(_.Owner >> Option.contains country)
        |> SupplyAction.execute country field
    
    Seq.append suppliedFromCities suppliedFromEdge
    |> Seq.distinct
