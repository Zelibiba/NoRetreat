module NoRetreat.Game.Cell
open NoRetreat
open NoRetreat.Game

open Avalonia.Input
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls


[<Struct>]
type City = 
    { Name: string
      Motherland: Country
      Owner: Country }

module Terrain =
    [<Struct>]
    type T =
        | Open
        | Forest
        | Marsh
        | Mountain
        | KerchStrait
        | City of City
        | Area

    let private createCity name =
        let country = 
            match name with
            | "Берлин"
            | "Познань"
            | "Кёнигсберг"
            | "Варшава"
            | "Бреслау"
            | "Прага"
            | "Вена"
            | "Люблин"
            | "Будапешт"
            | "Белград"
            | "Бухарест" -> Germany
            | _ -> USSR
        { Name = name
          Motherland = country
          Owner = country }

    let fromString str =
        match str with
        | "Open" -> Open
        | "Forest" -> Forest
        | "Marsh" -> Marsh
        | "Mountain" -> Mountain
        | "KerchStrait" -> KerchStrait
        | "Area" -> Area
        | _ -> createCity str |> City

    let cost terrain (unitType: Counter.UnitType) =
        match terrain with
        | Open -> 1
        | Forest -> if unitType.isTank then 2 else 1
        | Marsh -> if unitType.isTank then 4 else 2
        | Mountain
        | KerchStrait
        | Area -> if unitType.isTank then 3 else 2
        | City _ -> 1
        
        //|> ignore
        //0

    let getCity = function
        | City city -> city
        | terrain -> failwith (sprintf "Cell is %A, not City" terrain)

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

module ZOC =
    [<Struct>]
    type T = private { SumUSSR: int; SumGermany: int }

    let empty = { SumGermany = 0; SumUSSR = 0 }

    let isZOCOf country (zoc: T) =
        match country with
        | USSR -> zoc.SumUSSR > 0
        | Germany -> zoc.SumGermany > 0

    let isEZOCFor country (zoc: T) =
        match country with
        | USSR -> zoc.SumGermany > 0
        | Germany -> zoc.SumUSSR > 0

    let change quantity country (zoc: T) =
        match country with
        | USSR -> { zoc with SumUSSR = zoc.SumUSSR + quantity }
        | Germany -> { zoc with SumGermany = zoc.SumGermany + quantity}


[<Struct>]
type Selection =
    | NotSelected
    | CanBeDropped
    | CanMoveTo
    | MovedFrom

type T =
    { Coord: Coordinate
      Terrain: Terrain.T
      Rivers: Coordinate array
      Sea: Sea option
      BlockedSides: Coordinate array
      MapEdge: Country option

      Tower: Tower.T
      ZOC: ZOC.T
      Supply: Map<Country, bool>
      Selection: Selection }

      member x.belongsTo country = Option.contains country x.Tower.Owner

[<Struct>]
type Mask =
    | NoMask
    | ZOCMask
    | SupplyMask

[<Struct>]
type MaskInfo = MaskInfo of Mask * Country option

let belongsTo country (cell: T) = cell.belongsTo country

let unblockedDirsFrom adjacentCoords cell =
    adjacentCoords cell.Coord |> Array.except cell.BlockedSides

let selectedCounters cell = Tower.selectedCounters cell.Tower

let getMovementSelection cell (counter: Counter.T) =
    if Option.contains cell.Coord counter.MovedFrom then
        MovedFrom
    else if Terrain.cost cell.Terrain counter.Type <= counter.Movement.Remained then 
        CanMoveTo
    else
        NotSelected


type Msg =
    | TowerMsg of Tower.Msg
    | DragEntered
    | Dropped
    | ChangeZOC of Country * int
    | SetSelection of Selection
    | SetSupply of Country * bool

let update (msg: Msg) (state: T) : T =
    match msg with
    | TowerMsg towerMsg -> 
        { state with Tower = Tower.update towerMsg state.Tower }
    | DragEntered
    | Dropped -> state
    | ChangeZOC (country, quantity) ->
        { state with ZOC = ZOC.change quantity country state.ZOC }
    | SetSelection selection -> { state with Selection = selection }
    | SetSupply (country, isSupplied) ->
        { state with Supply = Map.add country isSupplied state.Supply }


let updateTower msg state =
    update (TowerMsg msg) state

let private backgroundSelection = function
    | CanMoveTo -> "green"
    | MovedFrom -> "red"
    | _ -> "transparent"

let private backgroundZOCFor (countryOpt: Country option) (zoc: ZOC.T) =
    match ZOC.isZOCOf USSR zoc, ZOC.isZOCOf Germany zoc with
    | true, true when countryOpt.IsNone -> "blue"
    | true, _    when not <| Option.contains Germany countryOpt -> "red"
    | _, true    when not <| Option.contains USSR countryOpt -> "gray"
    | _, _ -> "transparent"

let private backgroundSupply countryOpt (supply: Map<Country, bool>) =
    if supply[USSR] && supply[Germany] then
        match countryOpt with
        | Some USSR -> "red"
        | Some Germany -> "gray"
        | None -> "blue"
    else if supply[USSR] && not <| Option.contains Germany countryOpt then
        "red"
    else if supply[Germany] && not <| Option.contains USSR countryOpt then
        "gray"
    else
        "transparent"

let view (state: T, MaskInfo (mask, maskParam)) (dispatch: Msg -> unit) : IView =
    let radius = 92.2
    let computeX coord =
        1.73205080756 * radius * (float coord.C + (float coord.R) / 2.) + 2113.
    let computeY coord = 1.5 * radius * (float coord.R) + 1601.

    let checkDragDropArgs onSuccess (args: DragEventArgs) =
        if args.Data.Contains(DataFormats.Counters) then
            onSuccess args

    HexItem.create [ 
        HexItem.radius radius
        HexItem.left (computeX state.Coord)
        HexItem.top (computeY state.Coord)
        HexItem.backgroundOpacity 0.5
        HexItem.background (
            match mask with
            | NoMask -> backgroundSelection state.Selection
            | ZOCMask -> backgroundZOCFor maskParam state.ZOC
            | SupplyMask ->backgroundSupply maskParam state.Supply
        )
        //HexItem.borderThickness 1
        //HexItem.borderBrush "red"
        HexItem.clipToBounds false
        if not <| Array.isEmpty state.Tower.SelectedIdxs then
            HexItem.zIndex 1

        if state.Selection <> NotSelected then
            DragDrop.onDragEnter (checkDragDropArgs (fun _ -> dispatch DragEntered))
        if state.Selection = CanBeDropped then
            DragDrop.onDrop (checkDragDropArgs (fun _ -> dispatch Dropped))

    //    HexItem.content (Tower.view state.Tower (TowerMsg >> dispatch))
    //]
        HexItem.content (
            Panel.create [
                Panel.horizontalAlignment HorizontalAlignment.Center
                Panel.verticalAlignment VerticalAlignment.Center
                Panel.children [
                    TextBlock.create [
                        TextBlock.text (sprintf "%i, %i" state.Coord.R state.Coord.C)
                        TextBlock.fontSize 35
                    ]
                    Tower.view state.Tower (TowerMsg >> dispatch)
                ]
            ]
        ) 
    ]
