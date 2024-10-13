namespace NoRetreat.Game.Field
open NoRetreat
open NoRetreat.Game


[<Struct>]
type SelectionState =
    | NotSelected
    | Selected of coord: Coordinate
    | Dragging of origCoord: Coordinate * oldCoord: Coordinate * counters: Counter.T array

type T(countersSelection: SelectionState,

       map: Map<Coordinate, Cell.T>,
       citiesCoords: Coordinate list,
       mapEdgesCoords: Map<Country, Coordinate list>,

       mask: Cell.Mask,
       maskOptions: Map<Cell.Mask, Country option>) =

    static let linkedCoords =
        [| (-9, 13), (-10, 12)
           (5, -12), (3, -12)
           (7, -11), (5, -11) |]
        |> Array.map (Tuple.mapBoth Coordinate.create)
        |> Map
    static let realCoord coord = Map.tryFind coord linkedCoords |> Option.defaultValue coord

    new(pairs: (Coordinate * Cell.T) array) =
        let cities = 
            pairs |> Array.choose (fun (coord, cell) ->
                match cell.Terrain with
                | Cell.Terrain.City _ -> Some coord
                | _ -> None)
            |> Array.toList

        let edgeUSSR, edgeGermany =
            pairs |> Array.choose (fun (coord, cell) ->
                match cell.MapEdge with
                | Some country -> Some (coord, country)
                | None -> None)
            |> Array.partition (snd >> (=) USSR)
            |> Tuple.mapBoth (Array.map fst >> Array.toList)
        let mapEdges = Map [USSR, edgeUSSR; Germany, edgeGermany]

        let maskOptions = [(Cell.NoMask, None);
                           (Cell.ZOCMask, None);
                           (Cell.SupplyMask, None)] |> Map

        T(NotSelected, Map pairs, cities, mapEdges, Cell.NoMask, maskOptions)

    new(field: T, map, ?selection, ?citiesCoords, ?mapEdgesCoords, ?mask, ?maskOptions) =
        let selection = defaultArg selection field.Selection
        let citiesCoords = defaultArg citiesCoords field.CitiesCoords
        let mapEdgesCoords = defaultArg mapEdgesCoords field.MapEdgesCoords
        let mask = defaultArg mask field.Mask
        let maskOptions = defaultArg maskOptions field.MaskOptions

        T(selection, map, citiesCoords, mapEdgesCoords, mask, maskOptions)

    static member adjacentCoords coord =
        Map.tryFindKey (fun _ coord' -> coord' = coord) linkedCoords
        |> Option.map Coordinate.adjacentCoords
        |> Option.defaultValue [||]
        |> Array.append (Coordinate.adjacentCoords coord)

    member x.Item with get coord = x.getCell coord
    member _.getCell coord = realCoord coord |> map.get_Item
    member x.setCell coord updateCell =
        let map' = Map.change (realCoord coord) (Option.map updateCell) map
        T(x, map')

    member x.setSelection selection = if selection = countersSelection then x else T(x, map, selection=selection)
    member x.setMask mask' = if mask' = mask then x else T(x, map, mask=mask')
    member x.setMaskOptions maskOptions' = if maskOptions' = maskOptions then x else T(x, map, maskOptions=maskOptions')

    member _.Selection = countersSelection
    member _.Cells = map.Values
    member _.CitiesCoords = citiesCoords
    member _.MapEdgesCoords = mapEdgesCoords
    member _.Mask = mask
    member _.MaskOptions = maskOptions

    member x.CitiesCoordsOf country =
        List.filter (x.getCell >> _.Terrain >> Cell.Terrain.getCity >> _.Owner >> (=) country) citiesCoords

    member private _.contains coord =
        Map.containsKey coord map || Map.containsKey coord linkedCoords
    member private x.tryFind coord =
        Map.tryFind coord map
        |> Option.orElseWith (fun () ->
            Map.tryFind coord linkedCoords
            |> Option.map x.getCell)

    member x.choose fCoords (coord: Coordinate) =
        fCoords coord |> Array.choose x.tryFind


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


module Helpers =
    let setSelection selection (field: T) = field.setSelection selection
    let setMask mask (field: T) = field.setMask mask

    let adjacentCells coord (field: T) =
        field.choose T.adjacentCoords coord

    let unblockedCells coord (field: T) =
        field.choose (field.getCell >> Cell.unblockedDirsFrom T.adjacentCoords) coord

    let unblockedCellsWithItself coord field =
        [|
            yield! unblockedCells coord field
            yield field[coord]
        |]

    let updateCell msg coord (field: T) = field.setCell coord (Cell.update msg)
    let updateTower msg coord (field: T) = updateCell (Cell.TowerMsg msg) coord field

    let updateCells msg (field: T) coords = Array.foldBack (updateCell msg) coords field
    let updateTowers (field: T) msg coords = updateCells (Cell.TowerMsg msg) field coords

    let private changeZOC quantity country coord (field: T) =
        unblockedCellsWithItself coord field |> Array.map _.Coord
        |> updateCells (Cell.ChangeZOC (country, quantity)) field

    let addZOC quantity = changeZOC quantity
    let subZOC quantity = changeZOC -quantity

    let setCounters rawCoord idxs (field: T) =
        let coord = Coordinate.create rawCoord
        let counters = Array.map (Counter.init <| Some coord) idxs
        let owner = counters[0].Country

        updateCell (Cell.TowerMsg <| Tower.Init counters) coord field
        |> addZOC counters.Length owner coord