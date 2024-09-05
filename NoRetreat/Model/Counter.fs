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
type Country =
    | USSR
    | Germany

    member x.Opposite =
        match x with
        | USSR -> Germany
        | Germany -> USSR

    static member fromString(str) =
        match str with
        | "USSR" -> USSR
        | "Germany" -> Germany
        | _ -> failwithf "Can't parse to Country: %s" str

[<Struct>]
type Selection =
    | NotSelected
    | CanBeSelected
    | Selected


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
type CounterBuff =
    | NoBuff
    | OutOfSupply

type Counter =
    { CurrentSide: CounterSide
      OtherSide: CounterSide
      Movement: Movement
      Country: Country

      Buff: CounterBuff
      
      Selection: Selection
      IsSideSwapped: bool }

    member x.IsSelected = x.Selection = Selected
    member private x.unboxedCurrentSide = CounterSide.unbox x.CurrentSide
    member x.Type =  x.unboxedCurrentSide.Type
    member x.MovedFrom = List.tryHead x.Movement.History

module UnitData =
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

    let createCounter id =
        let unit = Array.find (fun (un: T.Unit) -> un.Id = id) data
        let fullSide = createSide unit.Sides[0]

        { CurrentSide = FullSide fullSide
          OtherSide = createSide unit.Sides[1] |> HalfSide
          Movement = Movement.create fullSide.MP 
          Country = Country.fromString unit.Country

          Buff = NoBuff
          
          Selection = CanBeSelected
          IsSideSwapped = false }

module Counter =
    module private Helpers =
        let swapSides counter =
            { counter with
                CurrentSide = counter.OtherSide
                OtherSide = counter.CurrentSide }

    let moveForward currentCoord cost counter =
        { counter.Movement with 
            Remained = counter.Movement.Remained - cost currentCoord counter.Type
            History = [currentCoord] @ counter.Movement.History }

    let moveBackward cost counter =
        { counter.Movement with 
            Remained = counter.Movement.Remained + cost counter.Movement.History[0] counter.Type
            History = counter.Movement.History[1..] }

    let init = UnitData.createCounter

    type Msg =
        | ChangeSelection of add: bool
        | Flip of back: bool
        | BeginDrag of PointerEventArgs
        | MoveCounter of (Counter -> Movement)

    let update (msg: Msg) (state: Counter) =
        match msg with
        | ChangeSelection _ ->
            match state.Selection with
                | NotSelected -> state
                | CanBeSelected -> { state with Selection = Selected }
                | Selected -> { state with Selection = CanBeSelected }
        | Flip back -> { state with IsSideSwapped = not back } |> Helpers.swapSides
        | BeginDrag _ -> state
        | MoveCounter updateUnit -> { state with Movement = updateUnit state }

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

    let private imageOOS = Bitmap.create "avares://NoRetreat/Assets/Images/OutOfSupply.PNG"

    let ImageOOS () = 
        Image.create [
            Image.height 80
            Image.width 80
            Image.source imageOOS 
        ] |> generalize

    let view (state: Counter) (dispatch: Msg -> unit) : IView =
        let size = 100
        DraggableBorder.create
            [ DraggableBorder.height size
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
                      splitByLeftButton,
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
                          match state.CurrentSide with
                          | FullSide unitInfo
                          | HalfSide unitInfo ->
                              Image.create [ Image.source (loadImage (state.Country, unitInfo)) ] |> generalize 
                          Border.create [
                              Border.horizontalAlignment HorizontalAlignment.Stretch
                              Border.verticalAlignment VerticalAlignment.Stretch
                              Border.cornerRadius 10
                              Border.borderThickness 5
                              Border.borderBrush (
                                  match state.Buff with
                                  | NoBuff -> "transparent"
                                  | OutOfSupply -> "orange")
                          ]
                      ]
                  ]   
              )
            ]
