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
    { Counters: Counter array
      SelectedIDs: int array
      IsExpanded: bool }

    member x.Item
        with get index = x.Counters[index]
        and set index value = x.Counters[index] <- value

    member x.Height = x.Counters.Length

    member x.Indexes = [| 0 .. x.Height - 1 |]

    member x.Owner =
        Array.tryPick (_.Info >> CounterInfo.unbox >> _.Country >> Some) x.Counters

module Tower =

    let create counters =
        { Counters = counters
          SelectedIDs = [||]
          IsExpanded = false }

    let none = create [||]

    let get (tower: Tower) idx = tower[idx]

    let setIsExpanded isExpanded tower = { tower with IsExpanded = isExpanded }

    let updateCounter msg idx (tower: Tower) =
        tower[idx] <- Counter.update msg tower[idx]
        tower

    let updateCounterAt (tower: Tower) msg idx =
        tower[idx] <- Counter.update msg tower[idx]

    let deselectCounters (tower: Tower) =
        Array.iter (updateCounterAt tower (Counter.ChangeSelection false)) tower.SelectedIDs

        { tower with SelectedIDs = [||] }

    let selectAllCounters (tower: Tower) =
        tower.Counters
        |> Array.indexed
        |> Array.filter (snd >> _.IsSelected >> not)
        |> Array.iter (fst >> updateCounterAt tower (Counter.ChangeSelection false))

        { tower with SelectedIDs = tower.Indexes }

    let defineSelection (tower: Tower) =
        let selectedIdxs =
            Array.filter (get tower >> _.IsSelected) tower.Indexes

        { tower with SelectedIDs = selectedIdxs }

    let selectedCounters (tower: Tower) =
        Array.map (get tower) tower.SelectedIDs

    let removeSelectedCounters (tower: Tower) =
        let counters' =
            Array.except tower.SelectedIDs tower.Indexes
            |> Array.map (get tower)

        let tower' =
            if counters'.Length <= 1 then
                setIsExpanded false tower
            else
                tower

        { tower' with
            Counters = counters'
            SelectedIDs = [||] }

    let addCounters newCounters (tower: Tower) =
        { tower with
            Counters = Array.append tower.Counters newCounters }

    let liftCounterUp (tower: Tower) =
        Array.sortDescending tower.SelectedIDs
        |> fun array -> if array[0] = tower.Height - 1 then array[1..] else array
        |> Array.iter (fun idx ->
            let counter = tower[idx]
            tower[idx] <- tower[idx + 1]
            tower[idx + 1] <- counter)

        tower

    let liftCounterDown (tower: Tower) =
        Array.sort tower.SelectedIDs
        |> fun array -> if array[0] = 0 then array[1..] else array
        |> Array.iter (fun idx ->
            let counter = tower[idx]
            tower[idx] <- tower[idx - 1]
            tower[idx - 1] <- counter)

        tower

[<Struct>]
type Terrain =
    | Open
    | Forest
    | Marsh
    | Mountain
    | KerchStrait
    | City of name: string

module Terrain =
    let fromString str =
        match str with
        | "Open" -> Open
        | "Forest" -> Forest
        | "Marsh" -> Marsh
        | "Mountain" -> Mountain
        | "KerchStrait" -> KerchStrait
        | _ -> City str

    let cost terrain (unitType: UnitType) =
        match terrain with
        | Open -> 1
        | Forest -> if unitType.isTank then 2 else 1
        | Marsh -> if unitType.isTank then 4 else 2
        | Mountain -> if unitType.isTank then 3 else 2
        | KerchStrait -> if unitType.isTank then 3 else 2
        | City _ -> 1

    let canMoveTo terrain (unit: UnitCounter) =
        cost terrain unit.Type <= unit.MP.Remained

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
type ZOC = { SumUSSR: int; SumGermany: int }

module ZOC =
    let empty = { SumGermany = 0; SumUSSR = 0 }

    let isEZOC country (zoc: ZOC) =
        match country with
        | USSR -> zoc.SumGermany > 0
        | Germany -> zoc.SumUSSR > 0

    let add country (zoc: ZOC) =
        match country with
        | USSR -> { zoc with SumUSSR = zoc.SumUSSR + 1 }
        | Germany ->
            { zoc with
                SumGermany = zoc.SumGermany + 1 }

    let sub country (zoc: ZOC) =
        match country with
        | USSR -> { zoc with SumUSSR = zoc.SumUSSR - 1 }
        | Germany ->
            { zoc with
                SumGermany = zoc.SumGermany - 1 }

[<Struct>]
type Selection =
    | NotSelected
    | CanBeDropped
    | CanMoveTo
    | MovedFrom

type Cell =
    { Coord: Coordinate
      Terrain: Terrain
      Rivers: Coordinate array
      Sea: Sea option
      BlockedSides: Coordinate array

      Tower: Tower
      ZOC: ZOC
      Selection: Selection }

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

          Tower = Tower.none
          ZOC = ZOC.empty
          Selection = NotSelected }

    let hexes =
        T.Load("avares://NoRetreat/Assets/Hexes.xml" |> Stream.create).Rows
        |> Array.collect (fun row -> Array.map (createfromHex row.Value >> (fun hex -> (hex.Coord, hex))) row.Hexs)
        |> dict

    let createFromCoords coord = hexes[coord]

module Cell =
    let setTower (cell: Cell) tower = { cell with Tower = tower }

    let unblockedDirections (cell: Cell) =
        Coordinate.adjacentCoords cell.Coord |> Seq.except cell.BlockedSides

    let selectedCounters (cell: Cell) = Tower.selectedCounters cell.Tower

    type Msg =
        | CounterMsg of int * Counter.Msg
        | ChangeTowerExpanded
        | DragEntered
        | Dropped
        | DeselectCounters
        | RemoveCounters
        | AddCounters of Counter array
        | LiftCounter of up: bool
        | AddZOC of Country
        | SubZOC of Country
        | SetSelection of Selection

    let update (msg: Msg) (state: Cell) =
        match msg with
        | CounterMsg(idx, counterMsg) ->
            match counterMsg with
            | Counter.ChangeSelection add when state.Tower.IsExpanded ->
                state.Tower
                |> if not add then Tower.deselectCounters else id
                |> Tower.updateCounter counterMsg idx
                |> Tower.defineSelection
                |> setTower state
            | Counter.ChangeSelection _ ->
                Array.iter (Tower.updateCounterAt state.Tower counterMsg) state.Tower.Indexes

                Tower.defineSelection state.Tower |> setTower state
            | _ -> Tower.updateCounter counterMsg idx state.Tower |> setTower state
        | ChangeTowerExpanded ->
            Tower.deselectCounters state.Tower
            |> Tower.setIsExpanded (not state.Tower.IsExpanded)
            |> setTower state
        | DragEntered
        | Dropped -> state
        | DeselectCounters -> state.Tower |> Tower.deselectCounters |> setTower state
        | RemoveCounters -> Tower.removeSelectedCounters state.Tower |> setTower state
        | AddCounters counters ->
            Tower.addCounters counters state.Tower
            |> if state.Tower.IsExpanded then id else Tower.selectAllCounters
            |> Tower.defineSelection
            |> setTower state
        | LiftCounter true ->
            Tower.liftCounterUp state.Tower
            |> Tower.defineSelection
            |> setTower state
        | LiftCounter false ->
            Tower.liftCounterDown state.Tower
            |> Tower.defineSelection
            |> setTower state
        | AddZOC country ->
            { state with
                ZOC = ZOC.add country state.ZOC }
        | SubZOC country ->
            { state with
                ZOC = ZOC.sub country state.ZOC }
        | SetSelection selection -> { state with Selection = selection }



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

    let view (state: Cell) (dispatch: Msg -> unit) : IView =
        let radius = 92.2

        let computeX coord =
            1.73205080756 * radius * (float coord.C + (float coord.R) / 2.) + 2113.

        let computeY coord = 1.5 * radius * (float coord.R) + 1601.

        let checkDragDropArgs onSuccess (args: DragEventArgs) =
            if args.Data.Contains(DataFormats.Counters) then
                onSuccess args

        HexItem.create
            [ HexItem.radius radius
              HexItem.left (computeX state.Coord)
              HexItem.top (computeY state.Coord)
              HexItem.backgroundOpacity 0.5
              HexItem.background (
                  match state.Selection with
                  | CanMoveTo -> "green"
                  | MovedFrom -> "red"
                  | _ -> "transparent"
              )
              //HexItem.borderThickness 1
              //HexItem.borderBrush "red"
              HexItem.clipToBounds false
              if not <| Array.isEmpty state.Tower.SelectedIDs then
                  HexItem.zIndex 1

              match state.Selection with
              | NotSelected -> ()
              | _ -> DragDrop.onDragEnter (checkDragDropArgs (fun _ -> dispatch DragEntered))
              match state.Selection with
              | CanBeDropped -> DragDrop.onDrop (checkDragDropArgs (fun _ -> dispatch Dropped))
              | _ -> ()

              HexItem.content (towerView state.Tower dispatch) ]
