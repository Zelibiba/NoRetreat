namespace NoRetreat.Controls

open Avalonia
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

        static member expandFactor<'t when 't :> TowerPanel>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(TowerPanel.ExpandFactorProperty, value, ValueNone)

        static member onIsExpandedChanged<'t when 't :> TowerPanel>(func: bool -> unit, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription<bool>(TowerPanel.IsExpandedProperty, func, ?subPatchOptions = subPatchOptions)

[<AutoOpen>]
module HexItem =
    let create (attrs: IAttr<HexItem> list) = ViewBuilder.Create<HexItem>(attrs)

    type HexItem with
        static member diagonal<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.DiagonalProperty, value, ValueNone)

        static member backgroundOpacity<'t when 't :> HexItem>(value: float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(HexItem.BackGroundOpacityProperty, value, ValueNone)