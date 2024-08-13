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

    static member fromString(str) =
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
type UnitInfo =
    { Name: string
      Type: UnitType
      Attack: AttackInfo
      Movement: int }

[<Struct>]
type CounterSide =
    | FullSide of fullSide: UnitInfo
    | HalfSide of halfSide: UnitInfo

[<Struct>]
type UnitCounter =
    { Country: Country
      CurrentSide: CounterSide
      OtherSide: CounterSide }

type CounterInfo = Unit of UnitCounter

type Counter =
    { IsSelected: bool
      IsSideSwapped: bool
      Counter: CounterInfo }

module UnitCounter =
    let swapSides unitCounter =
        { unitCounter with
            CurrentSide = unitCounter.OtherSide
            OtherSide = unitCounter.CurrentSide }

module UnitData =
    type T = XmlProvider<"""
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
        {
        Country = Country.fromString unit.Country
        CurrentSide = createSide unit.Sides[0] |> FullSide
        OtherSide = createSide unit.Sides[1] |> HalfSide
        }

module Counter =
    open UnitCounter

    let rand = System.Random()

    let init id =
        { IsSelected = false
          IsSideSwapped = false
          Counter = UnitData.createCounter id |> Unit }

    type Msg =
        | ChangeSelection of add: bool
        | Flip of back: bool
        | BeginDrag of PointerEventArgs
        | Dropped

    let update (msg: Msg) (state: Counter) =
        match msg with
        | ChangeSelection _ ->
            { state with
                IsSelected = not state.IsSelected }
        | Flip back ->
            let (Unit unitCounter) = state.Counter

            { state with
                Counter = swapSides unitCounter |> Unit
                IsSideSwapped = not back }
        | BeginDrag _
        | Dropped -> state

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

    let private loadImage country unitInfo =
        sprintf "avares://NoRetreat/Assets/Units/%A/%s.PNG" country unitInfo.Name
        |> Bitmap.create

    let Size = 100

    let view (state: Counter) (dispatch: Msg -> unit) : IView =
        let (Unit unit) = state.Counter

        DraggableBorder.create
            [ DraggableBorder.height Size
              DraggableBorder.width Size
              if state.IsSelected then
                  match unit.Country with
                  | USSR -> "Green"
                  | Germany -> "Red"
                  |> DraggableBorder.borderBrush

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
                      DraggableBorder.onDraggingStarted (_.PointerArgs >> BeginDrag >> dispatch)
                  else
                      DragDrop.allowDrop true
                      DragDrop.onDrop (EventLib.handled >> fun e ->
                          if e.Data.Contains(DataFormats.Counters) then dispatch Dropped)

              DraggableBorder.child (
                  match unit.CurrentSide with
                  | FullSide unitInfo
                  | HalfSide unitInfo -> Image.create [ Image.source (loadImage unit.Country unitInfo) ] |> generalize
              ) ]
