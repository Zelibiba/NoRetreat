namespace NoRetreat

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls

[<AutoOpen>]
module Image =
    let createWith (value: Avalonia.Media.IImage) =
        Image.create [Image.source value]

[<AutoOpen>]
module ScrollPane =
    let create (attrs: IAttr<ScrollPane> list) = ViewBuilder.Create<ScrollPane>(attrs)

[<AutoOpen>]
module TowerPanel =
    let create (attrs: IAttr<TowerPanel> list) = ViewBuilder.Create<TowerPanel>(attrs)

    type TowerPanel with
        static member deltaPadding<'t when 't :> TowerPanel>(value: Size) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Size>(TowerPanel.DeltaPaddingProperty, value, ValueNone)

        static member deltaPadding<'t when 't :> TowerPanel>(horizontal: float, vertical: float) : IAttr<'t> =
            Size(horizontal, vertical) |> TowerPanel.deltaPadding

[<AutoOpen>]
module HexItem =
    let create (attrs: IAttr<HexItem> list) = ViewBuilder.Create<HexItem>(attrs)

    type HexItem with
        static member radius<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.RadiusProperty, value, ValueNone)

        static member backgroundOpacity<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.BackGroundOpacityProperty, value, ValueNone)

[<AutoOpen>]
module DraggableBorder =
    let create (attrs: IAttr<DraggableBorder> list) = ViewBuilder.Create<DraggableBorder>(attrs)

    type DraggableBorder with
        static member sensitivity<'t when 't :> DraggableBorder>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(DraggableBorder.SensitivityProperty, value, ValueNone)

        static member onDraggingStarted<'t when 't :> DraggableBorder>(func: DraggingStartetEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DraggingStartetEventArgs>(DraggableBorder.DraggingStartedEvent, func, ?subPatchOptions = subPatchOptions)

[<AutoOpen>]
module MovableBorder =
    let create (attrs: IAttr<MovableBorder> list) = ViewBuilder.Create<MovableBorder>(attrs)