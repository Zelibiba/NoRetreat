namespace NoRetreat.Game.Field
open NoRetreat
open NoRetreat.Game

open System.Collections.Generic

type T =
    { UpdateValue: bool

      Cells: Dictionary<Coordinate, Cell.T> 
      CityCoords: Dictionary<Country, Coordinate list>
      MapEdgeCoords: Dictionary<Country, Coordinate array> 
      
      Mask: Cell.Mask
      MaskOptions: Map<Cell.Mask, Country option> }

    member private _._linkedCoords =
        [| (-9, 13), (-10, 12)
           (5, -12), (3, -12)
           (7, -11), (5, -11) |]
        |> Array.map (Tuple.map Coordinate.create Coordinate.create)
        |> Map

    member inline private x._realCoord coord =
         Map.tryFind coord x._linkedCoords
         |> Option.defaultValue coord

    member x.Item
        with get coord = x.Cells[x._realCoord coord]
        and set coord cell =x.Cells[x._realCoord coord] <- cell

    member x.get coord = x[coord]

    member x.contains coord =
        if x.Cells.ContainsKey(coord)
        then true
        else Map.containsKey coord x._linkedCoords

    member x.tryFind coord =
        match x.Cells.TryGetValue(coord) with
        | true, cell -> Some cell
        | false, _ -> Map.tryFind coord x._linkedCoords |> Option.map x.get

module private HexData =
    open FSharp.Data
    open System.IO
    open Cell

    type T =
        XmlProvider<
            Schema="""
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" attributeFormDefault="unqualified">
          <xs:element name="Hexes">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="row" maxOccurs="unbounded" minOccurs="0">
                  <xs:complexType>
                    <xs:sequence>
                      <xs:element name="hex" maxOccurs="unbounded" minOccurs="0">
                         <xs:complexType>
                           <xs:sequence>
                             <xs:element type="xs:string" name="river" maxOccurs="unbounded" minOccurs="0"/>
                             <xs:element type="xs:string" name="blocked" maxOccurs="unbounded" minOccurs="0"/>
                           </xs:sequence>
                           <xs:attribute type="xs:int" name="column" use="required"/>
                           <xs:attribute type="xs:string" name="type" use="required"/>
                           <xs:attribute type="xs:string" name="mapEdge" use="optional"/>
                           <xs:attribute type="xs:string" name="sea" use="optional"/>
                         </xs:complexType>
                      </xs:element>
                    </xs:sequence>
                    <xs:attribute type="xs:int" name="value" use="required"/>
                  </xs:complexType>
                </xs:element>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:schema>"""
         >

    let createfromHex row (hex: T.Hex) =
        let coord = Coordinate.create (row, hex.Column)

        { Coord = coord
          Terrain = Terrain.fromString hex.Type
          Rivers = Array.map (Coordinate.fromString >> ((+) coord)) hex.Rivers
          Sea = Option.map Sea.fromString hex.Sea
          BlockedSides = Array.map (Coordinate.fromString >> ((+) coord)) hex.Blockeds
          MapEdge = Option.map Country.fromString hex.MapEdge

          Tower = Tower.init [||]
          ZOC = ZOC.empty
          Supply = [(USSR, false); (Germany, false)] |> Map
          Selection = NotSelected }

    let hexes =
        T.Load("avares://NoRetreat/Assets/Hexes.xml" |> Stream.create).Rows
        |> Array.collect (fun row -> Array.map (createfromHex row.Value >> (fun hex -> (hex.Coord, hex))) row.Hexs)
        |> dict

module Helpers =
    let contains (field: T) coord = field.contains coord

    let tryFind (field: T) coord = field.tryFind coord

    let adjacentCoords (coord: Coordinate) =
        match coord.toTuple() with
        | -10, 12 -> seq { (-8, 12); (-8, 13) }
        | 3, -12  -> seq { (4, -11); (5, -11) }
        | 5, -11  -> seq { (6, -10) }
        | _, _ -> Seq.empty
        |> Seq.map Coordinate.create 
        |> Seq.append (Coordinate.adjacentCoords coord)

    let unblockedDirections coord (field: T) =
        Cell.unblockedDirsFor adjacentCoords field[coord] |> Seq.filter (contains field)

    let inline unblockedDirsWithItself coord field =
        seq {
            yield! unblockedDirections coord field
            yield coord
        }

    let create (dictionary: Dictionary<Coordinate, Cell.T>) =
        let citiesUSSR, citiesGermany =
            dictionary.Values
            |> Seq.choose (fun cell ->
                match cell.Terrain with
                | Cell.City city -> Some (cell.Coord, city.Owner)
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
        let mapEdges = [USSR, edgeUSSR; Germany, edgeGermany] |> dict |> Dictionary

        { UpdateValue = false
          Cells = dictionary 
          CityCoords = cities
          MapEdgeCoords = mapEdges 
          
          Mask = Cell.NoMask
          MaskOptions = [(Cell.NoMask, None);
                         (Cell.ZOCMask, None);
                         (Cell.SupplyMask, None)] |> Map }

    let update (field: T) = { field with UpdateValue = not field.UpdateValue }

    let updateCell msg coord (field: T) =
        field[coord] <- Cell.update msg field[coord]
        field

    let updateCellAt (field: T) msg coord =
        field[coord] <- Cell.update msg field[coord]

    let updateTowerAt (field: T) msg coord =
        field[coord] <- Cell.updateTower msg field[coord]

    let setCounters rawCoord idxs (field: T) =
        let coord = Coordinate.create rawCoord
        let counters = Array.map Counter.init idxs
        let owner = counters[0].Country

        field[coord] <- Tower.init counters |> Cell.setTower field[coord]
        unblockedDirsWithItself coord field 
        |> Seq.iter (updateCellAt field <| Cell.AddZOC (owner, counters.Length))
        field