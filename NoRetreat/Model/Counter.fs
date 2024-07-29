﻿namespace NoRetreat.Model

open System.IO
open Elmish
open FSharp.Data
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Helpers
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform
open Avalonia.Layout
open NoRetreat.Extentions
open NoRetreat.Controls
open NoRetreat.Controls.EventLib

[<Struct>]
type Country =
    | USSR
    | Germany

    static member fromString(str) =
        match str with
        | "USSR" -> USSR
        | "Germany" -> Germany
        | _ -> failwith "Wrong Country string"

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
        | _ -> failwith "Wrong UnitType string"

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
        | _ -> failwith "Wrong AttackType string"

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

type UnitData =
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

module UnitData =
    let data =
        UnitData.Load("avares://NoRetreat/Assets/UnitCounters.xml" |> Stream.create)

    let createSide (side: UnitData.Side) =
        { Name = side.Name
          Type = UnitType.fromString side.Type
          Attack =
            { Strength = side.Strength.Value
              Type = AttackType.fromString side.Strength.Type }
          Movement = side.Movement }

module Counter =
    open UnitCounter
    open UnitData

    type Msg =
        | SetIsSelected of bool
        | AddSelection of bool
        | Flip of back: bool

    let rand = System.Random()

    let init () =
        let idx = rand.Next(0, data.Units.Length)
        let unit = data.Units[idx]

        let country = Country.fromString unit.Country
        let fullSide = createSide unit.Sides[0] |> FullSide
        let halfSide = createSide unit.Sides[1] |> HalfSide

        { IsSelected = false
          IsSideSwapped = false
          Counter =
            Unit
                { Country = country
                  CurrentSide = fullSide
                  OtherSide = halfSide } }

    let update (msg: Msg) (state: Counter) =
        match msg with
        | SetIsSelected isSelected
        | AddSelection isSelected -> { state with IsSelected = isSelected }
        | Flip back ->
            let (Unit unitCounter) = state.Counter

            { state with
                Counter = swapSides unitCounter |> Unit
                IsSideSwapped = not back }

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
        |> Bitmap.Create

    let view (state: Counter) (dispatch: Msg -> unit) : IView =
        let (Unit unit) = state.Counter

        Border.create
            [ Border.height 70
              Border.width 70
              if state.IsSelected then
                  match unit.Country with
                  | USSR -> "Green"
                  | Germany -> "Red"
                  |> Border.borderBrush

                  Border.borderThickness 3
              else
                  Border.borderBrush "Black"
                  Border.borderThickness 1
              if state.IsSelected && not state.IsSideSwapped then
                  Border.zIndex 1
              else if state.IsSideSwapped then
                  Border.zIndex 2
              Border.cornerRadius 10
              Border.boxShadow (BoxShadow.Parse("-3 4 0 0 #515151"))

              if state.IsSideSwapped then
                  Border.onPointerReleased (fun e ->
                      e.Handled <- true
                      Flip true |> dispatch)
              else
                  Border.onPointerPressedExt2 (
                      splitByLeftButton,
                      (fun e ->
                          if e.KeyModifiers.HasFlag(Avalonia.Input.KeyModifiers.Control) then
                              AddSelection true |> dispatch
                          else
                              SetIsSelected true |> dispatch),
                      fun _ -> Flip false |> dispatch
                  )


              Border.child (
                  match unit.CurrentSide with
                  | FullSide unitInfo
                  | HalfSide unitInfo -> Image.create [ Image.source (loadImage unit.Country unitInfo) ] |> generalize
              ) ]
