module NoRetreat.Game.Counter
open NoRetreat
open NoRetreat.Controls

open System.IO
open FSharp.Data
open Avalonia.Layout
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media.Imaging
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls

[<Struct>]
type UnitType =
    | Infantry
    | Regional
    | Fortified
    | Shock
    | Mechanized
    | Tank
    | SSMechanized
    | SSTank

    member x.isTank =
        match x with
        | Tank
        | SSTank -> true
        | _ -> false

    static member fromString str =
        match str with
        | "Infantry" -> Infantry
        | "Regional" -> Regional
        | "Fortified" -> Fortified
        | "Shock" -> Shock
        | "Mechanized" -> Mechanized
        | "Tank" -> Tank
        | "SSMechanized" -> SSMechanized
        | "SSTank" -> SSTank
        | _ -> failwithf "Can't parse to UnitType: %s" str

[<Struct>]
type AttackType =
    | Usual
    | Passive
    | NoZOC

    static member fromString(str) =
        match str with
        | "Usual" -> Usual
        | "Passive" -> Passive
        | "NoZOC" -> NoZOC
        | _ -> failwithf "Can't parse to AttackType: %s" str

[<Struct>]
type AttackInfo = { Strength: int; Type: AttackType }

[<Struct>]
type SideInfo =
    { Name: string
      Type: UnitType
      Attack: AttackInfo
      MP: int }

[<Struct>]
type Side =
    | FullSide of fullSide: SideInfo
    | HalfSide of halfSide: SideInfo

    static member unbox counterSide =
        match counterSide with
        | FullSide side
        | HalfSide side -> side

[<Struct>]
type Movement = 
    { Full: int
      Remained: int 
      History: Coordinate list }

    static member create movementPoints coordOpt =
        let history = 
            Option.map List.singleton coordOpt
            |> Option.defaultValue []
        { Full = movementPoints
          Remained = movementPoints
          History = history }

[<Struct>]
type Marker =
    | NoMarker
    | OutOfSupply

type T =
    { CurrentSide: Side
      OtherSide: Side
      Movement: Movement
      Country: Country

      Markers: Marker
      
      Selection: Selection
      IsSideSwapped: bool }

    member x.IsSelected = x.Selection = Selected
    member x.MovedFrom = List.tryItem 1 x.Movement.History

    member private x.unboxedCurrentSide = Side.unbox x.CurrentSide
    member x.Type =  x.unboxedCurrentSide.Type
    member x.AttackType = x.unboxedCurrentSide.Attack.Type
    member x.MP = x.unboxedCurrentSide.MP

module private CounterLoader =
    type private T =
        XmlProvider<"""
        <Units>
			<Unit id="6" country="Germany">
				<Side name="1Panzer" type="Tank">
					<Strength type="Usual">7</Strength>
					<Movement>6</Movement>
				</Side>
				<Side name="1Panzer_h" type="Tank">
					<Strength type="Usual">5</Strength>
					<Movement>6</Movement>
				</Side>
			</Unit>
		    <Unit id="1" country="Germany">
			    <Side name="2Panzer" type="Tank">
				    <Strength type="Usual">7</Strength>
				    <Movement>6</Movement>
			    </Side>
			    <Side name="2Panzer_h" type="Tank">
				    <Strength type="Usual">5</Strength>
				    <Movement>6</Movement>
			    </Side>
		    </Unit>
		</Units>""">

    let private data =
        T.Load("avares://NoRetreat/Assets/UnitCounters.xml" |> Stream.create).Units

    let private createSide (side: T.Side) =
        { Name = side.Name
          Type = UnitType.fromString side.Type
          Attack =
            { Strength = side.Strength.Value
              Type = AttackType.fromString side.Strength.Type }
          MP = side.Movement }

    let createCounter currentCoordOpt id =
        let unit = Array.find (fun (un: T.Unit) -> un.Id = id) data
        let fullSide = createSide unit.Sides[0]

        { CurrentSide = FullSide fullSide
          OtherSide = createSide unit.Sides[1] |> HalfSide
          Movement = Movement.create fullSide.MP currentCoordOpt
          Country = Country.fromString unit.Country

          Markers = NoMarker
          
          Selection = CanBeSelected
          IsSideSwapped = false }

module private Helpers =
    let swapSides counter = { counter with
                                CurrentSide = counter.OtherSide
                                OtherSide = counter.CurrentSide }

    let setMarker (counter: T) =
        let movement' = { counter.Movement with Full = 3; Remained = 3 }
        { counter with Markers = OutOfSupply
                       Movement = movement' }

    let removeMarker (counter: T) =
        let mp = counter.MP
        let movement' = { counter.Movement with Full = mp; Remained = mp }
        {counter with Markers = NoMarker
                      Movement = movement' }

    let hasZOC (counter: T) =
        counter.AttackType <> NoZOC && counter.Markers <> OutOfSupply


let init = CounterLoader.createCounter

let getZOCModification (counters: T array) =
    let owner = counters[0].Country
    let withZOC = Array.filter Helpers.hasZOC counters
    owner, withZOC.Length

type Msg =
    | ChangeSelection of add: bool
    | Flip of back: bool
    | BeginDrag of PointerEventArgs
    | MoveCounter of cost: (UnitType -> Coordinate -> int) * newCoord: Coordinate
    | SetSupply of bool

let update (msg: Msg) (state: T) =
    match msg with
    | ChangeSelection _ ->
        match state.Selection with
            | NotSelected -> state
            | CanBeSelected -> { state with Selection = Selected }
            | Selected -> { state with Selection = CanBeSelected }
    | Flip back -> { state with IsSideSwapped = not back } |> Helpers.swapSides
    | BeginDrag _ -> state
    | MoveCounter (cost, nextCoord) ->
        let cost' = cost state.Type
        let movement = state.Movement
        let movement' =
            if Option.contains nextCoord state.MovedFrom then
                { movement with
                    Remained = movement.Remained + cost' movement.History[0]
                    History = movement.History.Tail }
            else
                { movement with
                    Remained = movement.Remained - cost' nextCoord
                    History = nextCoord :: movement.History }

        { state with Movement = movement' }
    | SetSupply isSupplied -> (if isSupplied then Helpers.removeMarker  else Helpers.setMarker) <| state

module private Images =
    let load =
        Library.memoize
        <| fun (country: Country, unitInfo) ->
            sprintf "avares://NoRetreat/Assets/Units/%A/%s.PNG" country unitInfo.Name
            |> Bitmap.create

    let outOfSupply = Bitmap.create "avares://NoRetreat/Assets/Images/OutOfSupply.PNG"

let ImageOOS () = 
    Image.create [
        Image.height 80
        Image.width 80
        Image.source Images.outOfSupply 
    ] |> generalize

let view (state: T) (dispatch: Msg -> unit) : IView =
    let size = 100
    DraggableBorder.create [ 
        DraggableBorder.height size
        DraggableBorder.width size
        DraggableBorder.cornerRadius 15
        //DraggableBorder.boxShadow (BoxShadow.Parse("-3 4 0 0 #515151"))

        match state.Selection with
        | NotSelected -> 
            DraggableBorder.borderBrush "Black"
            DraggableBorder.borderThickness 1
        | CanBeSelected ->
            DraggableBorder.borderBrush (
                match state.Country with
                | USSR -> "Green"
                | Germany -> "Red")
            DraggableBorder.borderThickness 5
        | Selected ->
            DraggableBorder.borderBrush "Black"
            DraggableBorder.borderThickness 5
            if not state.IsSideSwapped then
                DraggableBorder.zIndex 1
                DraggableBorder.sensitivity 20
                DraggableBorder.onDraggingStarted (EventLib.handled >> _.PointerArgs >> BeginDrag >> dispatch)

        if state.IsSideSwapped then
            DraggableBorder.zIndex 2
            DraggableBorder.onPointerReleased (EventLib.handled >> fun _ -> Flip true |> dispatch)
        else
            DraggableBorder.onPointerPressedExt2 (
                EventLib.splitByLeftButton,
                (fun e ->
                    if e.KeyModifiers.HasFlag(KeyModifiers.Control) then
                        dispatch (ChangeSelection true)
                    else if not state.IsSelected then
                        dispatch (ChangeSelection false)),
                (fun _ -> Flip false |> dispatch),
                SubPatchOptions.OnChangeOf state.Selection
            )

        DraggableBorder.child (
            Panel.create [
                Panel.children [
                    Image.create [ (state.Country, Side.unbox state.CurrentSide) |> Images.load |> Image.source ] |> generalize
                    Border.create [
                        Border.horizontalAlignment HorizontalAlignment.Stretch
                        Border.verticalAlignment VerticalAlignment.Stretch
                        Border.cornerRadius 10
                        Border.borderThickness 7
                        Border.borderBrush (
                            match state.Markers with
                            | NoMarker -> "transparent"
                            | OutOfSupply -> "orange")
                    ]
                ]
            ]   
        )
    ]
