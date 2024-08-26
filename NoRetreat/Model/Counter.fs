namespace NoRetreat

open System.IO
open FSharp.Data
open Avalonia.Layout
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open NoRetreat.Controls
open NoRetreat.Controls.EventLib
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

module UnitType =
    let fromString str =
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

    let isTank (unitType: UnitType) = unitType.isTank

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
      Movement: int }

[<Struct>]
type CounterSide =
    | FullSide of fullSide: SideInfo
    | HalfSide of halfSide: SideInfo

    static member unbox counterSide =
        match counterSide with
        | FullSide side
        | HalfSide side -> side

[<Struct>]
type Movement = 
    { Current: int
      Remained: int 
      History: Coordinate list }

module Movement =
    let create movementPoints =
        { Current = movementPoints
          Remained = movementPoints
          History = [] }

[<Struct>]
type Unit =
    { CurrentSide: CounterSide
      OtherSide: CounterSide
      Country: Country
      MP: Movement }

    member private x.unboxedCurrentSide = CounterSide.unbox x.CurrentSide
    member x.Type =  x.unboxedCurrentSide.Type
    member x.MovedFrom = List.tryHead x.MP.History

module Unit =
    let swapSides unit =
        { unit with
            CurrentSide = unit.OtherSide
            OtherSide = unit.CurrentSide }

    let moveForward oldCoord cost (unit: Unit) =
        let mp = { unit.MP with 
                    Remained = unit.MP.Remained - cost unit.Type
                    History = [oldCoord] @ unit.MP.History }
        { unit with MP = mp }

    let moveBackward cost (unit: Unit) =
        let mp = { unit.MP with 
                    Remained = unit.MP.Remained + cost unit.Type
                    History = unit.MP.History[1..] }
        { unit with MP = mp }


type CounterInfo = Unit of Unit

module CounterInfo =
    let unbox (Unit unit) = unit
    let map f = unbox >> f >> Unit

type Counter =
    { IsSelected: bool
      IsSideSwapped: bool
      Info: CounterInfo }

module UnitData =
    type T =
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

    let data =
        T.Load("avares://NoRetreat/Assets/UnitCounters.xml" |> Stream.create).Units

    let createSide (side: T.Side) =
        { Name = side.Name
          Type = UnitType.fromString side.Type
          Attack =
            { Strength = side.Strength.Value
              Type = AttackType.fromString side.Strength.Type }
          Movement = side.Movement }

    let createCounter id =
        let unit = Array.find (fun (un: T.Unit) -> un.Id = id) data
        let fullSide = createSide unit.Sides[0]

        { CurrentSide = FullSide fullSide
          OtherSide = createSide unit.Sides[1] |> HalfSide
          Country = Country.fromString unit.Country
          MP = Movement.create fullSide.Movement }

module Counter =
    module private Helpers =
        let updateInfo f (counter: Counter) =
            { counter with Info = f counter.Info }
        let map = CounterInfo.map >> updateInfo

    let init id =
        { IsSelected = false
          IsSideSwapped = false
          Info = UnitData.createCounter id |> Unit }

    type Msg =
        | ChangeSelection of add: bool
        | Flip of back: bool
        | BeginDrag of PointerEventArgs
        | UpdateUnit of (Unit -> Unit)

    let update (msg: Msg) (state: Counter) =
        match msg with
        | ChangeSelection _ ->
            { state with
                IsSelected = not state.IsSelected }
        | Flip back ->
            { state with IsSideSwapped = not back }
            |> Helpers.map Unit.swapSides
        | BeginDrag _ -> state
        | UpdateUnit updateUnit -> Helpers.map updateUnit state

    //let iconView unitType : IView =
    //    let height = 20.
    //    let width = 30.
    //    let thickness = 2.
    //    Panel.create [
    //        Panel.dock Dock.Top
    //        Panel.height height
    //        Panel.width width
    //        Panel.children [
    //            Rectangle.create [
    //                    Rectangle.horizontalAlignment HorizontalAlignment.Stretch
    //                    Rectangle.verticalAlignment VerticalAlignment.Stretch
    //                    Rectangle.fill "#CEDFD6"
    //                    Rectangle.stroke "Black"
    //                    Rectangle.strokeThickness thickness
    //                ]
    //            match unitType with
    //            | Infratry ->
    //                Line.create [
    //                    Line.startPoint (thickness, thickness)
    //                    Line.endPoint (width - thickness, height - thickness)
    //                    Line.stroke "Black"
    //                    Line.strokeThickness thickness
    //                ]
    //                Line.create [
    //                    Line.startPoint (width - thickness, thickness)
    //                    Line.endPoint (thickness, height - thickness)
    //                    Line.stroke "Black"
    //                    Line.strokeThickness thickness
    //                ]
    //            | _ -> TextBlock.create [ TextBlock.text "UnitType not found" ]
    //        ]
    //    ]
    //
    //let unitView (unit: UnitInfo) (dispatch: Msg -> unit) : IView =
    //    DockPanel.create [
    //        DockPanel.horizontalAlignment HorizontalAlignment.Center
    //        DockPanel.verticalAlignment VerticalAlignment.Stretch
    //        DockPanel.margin 5
    //        DockPanel.children [
    //            TextBlock.create [
    //                TextBlock.dock Dock.Top
    //                TextBlock.text unit.Name
    //                TextBlock.fontSize 10
    //            ]
    //            iconView unit.Type
    //            StackPanel.create [
    //                StackPanel.dock Dock.Bottom
    //                StackPanel.orientation Orientation.Horizontal
    //                StackPanel.children [
    //                ]
    //            ]
    //        ]
    //    ]

    let private loadImage =
        Lib.memoize
        <| fun (country: Country, unitInfo) ->
            sprintf "avares://NoRetreat/Assets/Units/%A/%s.PNG" country unitInfo.Name
            |> Bitmap.create

    let Size = 100

    let view (state: Counter) (dispatch: Msg -> unit) : IView =
        let (Unit unit) = state.Info

        DraggableBorder.create
            [ DraggableBorder.height Size
              DraggableBorder.width Size
              if state.IsSelected then
                  DraggableBorder.borderBrush (
                      match unit.Country with
                      | USSR -> "Green"
                      | Germany -> "Red")

                  DraggableBorder.borderThickness 3
              else
                  DraggableBorder.borderBrush "Black"
                  DraggableBorder.borderThickness 1
              if state.IsSelected then
                  if state.IsSideSwapped then
                      DraggableBorder.zIndex 2
                  else
                      DraggableBorder.zIndex 1
              DraggableBorder.cornerRadius 15
              //DraggableBorder.boxShadow (BoxShadow.Parse("-3 4 0 0 #515151"))

              if state.IsSideSwapped then
                  DraggableBorder.onPointerReleased (EventLib.handled >> fun _ -> Flip true |> dispatch)
              else
                  DraggableBorder.onPointerPressedExt2 (
                      splitByLeftButton,
                      (fun e ->
                          if e.KeyModifiers.HasFlag(KeyModifiers.Control) then
                              dispatch (ChangeSelection true)
                          else if not state.IsSelected then
                              dispatch (ChangeSelection false)),
                      (fun _ -> Flip false |> dispatch),
                      SubPatchOptions.OnChangeOf state.IsSelected
                  )

                  if state.IsSelected then
                      DraggableBorder.sensitivity 20
                      DraggableBorder.onDraggingStarted (EventLib.handled >> _.PointerArgs >> BeginDrag >> dispatch)

              DraggableBorder.child (
                  match unit.CurrentSide with
                  | FullSide unitInfo
                  | HalfSide unitInfo ->
                      Image.create [ Image.source (loadImage (unit.Country, unitInfo)) ] |> generalize
              ) ]
