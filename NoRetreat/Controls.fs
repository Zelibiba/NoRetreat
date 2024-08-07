namespace NoRetreat.Controls

open Avalonia
open Avalonia.Input
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open HexGameControls

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
        static member diagonal<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.DiagonalProperty, value, ValueNone)

        static member backgroundOpacity<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.BackGroundOpacityProperty, value, ValueNone)

[<AutoOpen>]
module DraggableControl =
    let create (attrs: IAttr<DraggableControl> list) = ViewBuilder.Create<DraggableControl>(attrs)

    type DraggableControl with
        static member sensitivity<'t when 't :> DraggableControl>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(DraggableControl.SensitivityProperty, value, ValueNone)

        static member onDraggingStarted<'t when 't :> DraggableControl>(func: DraggingStartetEventArgs -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<DraggingStartetEventArgs>(DraggableControl.DraggingStartedEvent, func, ?subPatchOptions = subPatchOptions)