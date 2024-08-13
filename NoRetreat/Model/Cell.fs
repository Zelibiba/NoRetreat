namespace NoRetreat

open System.IO
open System.Collections.Generic
open FSharp.Data
open Avalonia.Input
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open NoRetreat.Controls
open HexGameControls

type Tower =
    { UpdateField: bool
      Counters: Counter array
      IsExpanded: bool
      IsSelected: bool }

    member this.Item
        with get index = this.Counters[index]
        and set index value = this.Counters[index] <- value

module Tower =

    let create counters =
        { UpdateField = false
          Counters = counters
          IsExpanded = false
          IsSelected = false }

    let none = create [||]

    let get (tower: Tower) idx = tower[idx]

    let setIsExpanded isExpanded tower = { tower with IsExpanded = isExpanded }

    let updateCounter msg idx (tower: Tower) =
        tower[idx] <- Counter.update msg tower[idx]
        tower

    let updateCounterAt tower msg idx = updateCounter msg idx tower |> ignore

    let deselectCounters (tower: Tower) =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with IsSelected = false }

    let selectAllCounters (tower: Tower) =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected >> not)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with IsSelected = true }

    let defineSelection (tower: Tower) =
        let selectedIdxs =
            tower.Counters
            |> Array.indexed
            |> Array.filter (snd >> _.IsSelected)
            |> Array.map fst

        { tower with
            IsSelected = Array.isEmpty selectedIdxs |> not },
        selectedIdxs

    let removeCounters idxs (tower: Tower) =
        let removed = Array.map tower.get_Item idxs
        let counters' = Array.except removed tower.Counters

        let tower' =
            if counters'.Length <= 1 then
                setIsExpanded false tower
            else
                tower

        { tower' with
            Counters = counters'
            IsSelected = false },
        removed

    let addCounters targetIdx newCounters (tower: Tower) =
        let counters' =
            match targetIdx with
            | Some idx -> Array.insertManyAt (idx + 1) newCounters tower.Counters
            | None -> Array.append tower.Counters newCounters

        { tower with Counters = counters' }

[<Struct>]
type Terrain =
    | Open
    | Forest
    | Marsh
    | Mountain
    | KerchStrait
    | City of name: string

    static member fromString(str) =
        match str with
        | "Open" -> Open
        | "Forest" -> Forest
        | "Marsh" -> Marsh
        | "Mountain" -> Mountain
        | "KerchStrait" -> KerchStrait
        | _ -> City str

[<Struct>]
type Sea =
    | Baltic
    | Black
    | Adriatic
    | Caspian

    static member fromString(str) =
        match str with
        | "Baltic" -> Baltic
        | "Black" -> Black
        | "Adriatic" -> Adriatic
        | "Caspian" -> Caspian
        | _ -> failwithf "Can't parse to Sea: %s" str

[<Struct>]
type ZOC =
    | NoZOC
    | One of Country
    | Both

    static member add (zoc: ZOC) (owner: Country option) =
        match zoc, owner with
        | _, None -> zoc
        | NoZOC, Some country -> One country
        | One country1, Some country2 when country1 = country2 -> zoc
        | _, Some _ -> Both


type Cell =
    { Coord: Coordinate
      Terrain: Terrain
      Rivers: Coordinate array
      Sea: Sea option
      BlockedSides: Coordinate array

      CanBeSelected: bool
      Owner: Country option
      ZOC: ZOC
      Tower: Tower }

module HexData =
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
        let edge = Option.map Country.fromString hex.MapEdge
        let coord = Coordinate.create (row, hex.Column)

        { Coord = coord
          Terrain = Terrain.fromString hex.Type
          Rivers = Array.map (Coordinate.fromString >> ((+) coord)) hex.Rivers
          Sea = Option.map Sea.fromString hex.Sea
          BlockedSides = Array.map (Coordinate.fromString >> ((+) coord)) hex.Blockeds

          CanBeSelected = false
          Owner = None
          ZOC = NoZOC
          Tower = Tower.none }

    let hexes =
        T.Load("avares://NoRetreat/Assets/Hexes.xml" |> Stream.create).Rows
        |> Array.collect (fun row -> Array.map (createfromHex row.Value >> (fun hex -> (hex.Coord, hex))) row.Hexs)
        |> dict

    let createFromCoords coord = hexes[coord]

module Cell =
    module Helpers =
        let setTower (cell: Cell) tower = { cell with Tower = tower }

        let setTowerWithOwnerCheck (cell: Cell) tower =
            let owner =
                Array.tryPick
                    (fun counter ->
                        let (Unit unit) = counter.Counter
                        Some unit.Country)
                    tower.Counters

            { cell with
                Tower = tower
                Owner = owner }

        let unblockedDirections (cell: Cell) =
            Coordinate.adjacentCoords cell.Coord |> Seq.except cell.BlockedSides

    type Msg =
        | CounterMsg of int * Counter.Msg
        | ChangeTowerExpanded
        | Dropped
        | DeselectCounters
        | RemoveCounters of int array
        | AddCounters of int option * Counter array
        | SetZOC of ZOC

    type ExtraData =
        | SelectedIdxs of int array
        | RemovedCounters of Counter array
        | NoData

    let update (msg: Msg) (state: Cell) =
        match msg with
        | CounterMsg(idx, counterMsg) ->
            match counterMsg with
            | Counter.ChangeSelection add when state.Tower.IsExpanded ->
                state.Tower
                |> if not add then Tower.deselectCounters else id
                |> Tower.updateCounter counterMsg idx
                |> Tower.defineSelection
                |> Tuple.map (Helpers.setTower state) SelectedIdxs
            | Counter.ChangeSelection _ ->
                Array.init state.Tower.Counters.Length id
                |> Array.iter (Tower.updateCounterAt state.Tower counterMsg)

                Tower.defineSelection state.Tower
                |> Tuple.map (Helpers.setTower state) SelectedIdxs
            | _ -> Tower.updateCounter counterMsg idx state.Tower |> Helpers.setTower state, NoData
        | ChangeTowerExpanded ->
            state.Tower
            |> Tower.deselectCounters
            |> Tower.setIsExpanded (not state.Tower.IsExpanded)
            |> Helpers.setTower state,
            NoData
        | Dropped -> state, NoData
        | DeselectCounters -> state.Tower |> Tower.deselectCounters |> Helpers.setTower state, NoData
        | RemoveCounters idxs ->
            state.Tower
            |> Tower.removeCounters idxs
            |> Tuple.map (Helpers.setTowerWithOwnerCheck state) RemovedCounters
        | AddCounters(idxOpt, counters) ->
            state.Tower
            |> Tower.addCounters idxOpt counters
            |> if state.Tower.IsExpanded then
                   id
               else
                   Tower.selectAllCounters
            |> Tower.defineSelection
            |> Tuple.map (Helpers.setTowerWithOwnerCheck state) SelectedIdxs
        | SetZOC zoc -> { state with ZOC = zoc }, NoData


    let towerView (state: Tower) (dispatch: Msg -> unit) : IView =
        let dispatchCounter = Library.dispatchwithIndex dispatch CounterMsg

        TowerPanel.create
            [ //TowerPanel.height Counter.Size
              //TowerPanel.width Counter.Size
              TowerPanel.horizontalAlignment HorizontalAlignment.Center
              TowerPanel.verticalAlignment VerticalAlignment.Center
              if state.IsExpanded then
                  TowerPanel.deltaPadding (17.5, -38.5)
              else
                  TowerPanel.deltaPadding (5, -11)
              TowerPanel.onDoubleTapped (fun _ -> dispatch ChangeTowerExpanded)
              TowerPanel.children (
                  state.Counters
                  |> Array.mapi (fun idx counter -> Counter.view counter (dispatchCounter idx))
                  |> Array.toList
              ) ]

    let diagonal = 92.2

    let computeX coord =
        1.73205080756 * diagonal * (float coord.C + (float coord.R) / 2.) + 2113.

    let computeY coord =
        1.5 * diagonal * (float coord.R) + 1601.

    let view (state: Cell) (dispatch: Msg -> unit) : IView =
        HexItem.create
            [ HexItem.radius diagonal
              HexItem.left (computeX state.Coord)
              HexItem.top (computeY state.Coord)
              HexItem.background ("transparent")
              //HexItem.backgroundOpacity 0.5
              //HexItem.borderThickness 3
              //HexItem.borderBrush "red"
              HexItem.clipToBounds false
              if state.Tower.IsSelected then
                  HexItem.zIndex 1

              DragDrop.allowDrop true
              //DragDrop.onDragEnter ((fun _ -> System.Diagnostics.Trace.WriteLine(state.Coord)), SubPatchOptions.OnChangeOf state.Coord)
              DragDrop.onDrop (fun e ->
                  if e.Data.Contains(DataFormats.Counters) then
                      dispatch Dropped)

              HexItem.content (towerView state.Tower dispatch) ]
