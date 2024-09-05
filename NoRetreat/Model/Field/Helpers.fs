module NoRetreat.Field.Helpers
open NoRetreat
open System.Collections.Generic


let inline contains (field: Field) coord = field.Cells.ContainsKey coord

let inline get (field: Field) coord = field[coord]

let tryFind (field: Field) coord =
    match field.Cells.TryGetValue(coord) with
    | true, cell -> Some cell
    | false, _ -> None

let inline unblockedDirections coord (field: Field) =
    Cell.unblockedDirections field[coord] |> Seq.filter (contains field)

let inline unblockedDirsWithItself coord field =
    seq {
        yield! unblockedDirections coord field
        yield coord
    }

let create (dictionary: Dictionary<Coordinate,Cell>) =
    let citiesUSSR, citiesGermany =
        dictionary.Values
        |> Seq.choose (fun cell ->
            match cell.Terrain with
            | City city -> Some (cell.Coord, city.Owner)
            | _ -> None)
        |> Seq.toList
        |> List.partition (snd >> (=) USSR)
        |> Tuple.map (List.map fst) (List.map fst)
    let cities = [USSR, citiesUSSR; Germany, citiesGermany] |> dict |> Dictionary

    let edgeUSSR, edgeGermany =
        dictionary.Values
        |> Seq.choose (fun cell ->
            match cell.MapEdge with
            | Some country -> Some (cell.Coord, country)
            | None -> None)
        |> Seq.toArray
        |> Array.partition (snd >> (=) USSR)
        |> Tuple.map (Array.map fst) (Array.map fst)
    //let edgeUSSR = [| 14; |] |> Array.map (fun i -> Coordinate.create (-8,i))
    let mapEdges = [USSR, edgeUSSR; Germany, edgeGermany] |> dict |> Dictionary

    { UpdateValue = false
      Cells = dictionary 
      CityCoords = cities
      MapEdgeCoords = mapEdges}

let update (field: Field) = { field with UpdateValue = not field.UpdateValue }

let updateCell msg coord (field: Field) =
    field[coord] <- Cell.update msg field[coord]
    field

let updateCellAt (field: Field) msg coord =
    field[coord] <- Cell.update msg field[coord]

let setCounters rawCoord idxs (field: Field) =
    let coord = Coordinate.create rawCoord
    let counters = Array.map Counter.init idxs
    let owner = counters[0].Country

    field[coord] <- Tower.create counters |> Cell.setTower field[coord]
    unblockedDirsWithItself coord field 
    |> Seq.iter (updateCellAt field <| Cell.AddZOC (owner, counters.Length))
    field