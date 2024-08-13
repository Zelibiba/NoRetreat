namespace NoRetreat.Controls

open System
open System.Threading
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

[<AutoOpen>]
module EventExtentions =

    type IObservable<'a> with
        member this.Subscribe(callback: 'a -> unit, token: CancellationToken) =
            let disposable = Observable.subscribe callback this
            token.Register(fun () -> disposable.Dispose()) |> ignore

    type SubPatchOptions with
        member this.ToScope() : obj =
            match this with
            | Always -> Guid.NewGuid() :> _
            | Never -> null
            | OnChangeOf t -> t

    type AttrBuilder<'view> with
        static member CreateSubscriptionExt<'arg when 'arg :> RoutedEventArgs>
            (
                routedEvent: RoutedEvent<'arg>,
                eventMapper: IObservable<'arg> -> IObservable<'arg>,
                func: 'arg -> unit,
                ?subPatchOptions: SubPatchOptions
            ) : IAttr<'view> =

            let subscribeFunc (control: Control, _handler: 'h) =
                let cts = new CancellationTokenSource()
                let observable = control.GetObservable(routedEvent) |> eventMapper
                observable.Subscribe(func, cts.Token)
                cts

            let attr =
                Attr<'view>.Subscription
                    { Name = routedEvent.Name + ".RoutedEventSub"
                      Subscribe = subscribeFunc
                      Func = Action<_>(func)
                      FuncType = func.GetType()
                      Scope = (Option.defaultValue SubPatchOptions.Never subPatchOptions).ToScope() }

            attr :> IAttr<'view>

        static member CreateSubscriptionExt2<'arg when 'arg :> RoutedEventArgs>
            (
                routedEvent: RoutedEvent<'arg>,
                eventMapper: IObservable<'arg> -> IObservable<'arg> * IObservable<'arg>,
                func1: 'arg -> unit,
                func2: 'arg -> unit,
                ?subPatchOptions: SubPatchOptions
            ) : IAttr<'view> =

            let subscribeFunc (control: Control, _handler: 'h) =
                let cts = new CancellationTokenSource()
                let observable1, observable2 = control.GetObservable(routedEvent) |> eventMapper
                observable1.Subscribe(func1, cts.Token)
                observable2.Subscribe(func2, cts.Token)
                cts

            let attr =
                Attr<'view>.Subscription
                    { Name = routedEvent.Name + ".RoutedEventSub"
                      Subscribe = subscribeFunc
                      Func = Action<_>(func1)
                      FuncType = func1.GetType()
                      Scope = (Option.defaultValue SubPatchOptions.Never subPatchOptions).ToScope() }

            attr :> IAttr<'view>

    type InputElement with
        static member onPointerPressedExt<'t when 't :> InputElement>
            (
                mapper: IObservable<PointerPressedEventArgs> -> IObservable<PointerPressedEventArgs>,
                func: PointerPressedEventArgs -> unit,
                ?subPatchOptions
            ) =
            AttrBuilder<'t>
                .CreateSubscriptionExt<PointerPressedEventArgs>(
                    InputElement.PointerPressedEvent,
                    mapper,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

        static member onPointerPressedExt2<'t when 't :> InputElement>
            (
                mapper:
                    IObservable<PointerPressedEventArgs>
                        -> IObservable<PointerPressedEventArgs> * IObservable<PointerPressedEventArgs>,
                func1: PointerPressedEventArgs -> unit,
                func2: PointerPressedEventArgs -> unit,
                ?subPatchOptions
            ) =
            AttrBuilder<'t>
                .CreateSubscriptionExt2<PointerPressedEventArgs>(
                    InputElement.PointerPressedEvent,
                    mapper,
                    func1,
                    func2,
                    ?subPatchOptions = subPatchOptions
                )

        static member onPointerReleasedExt<'t when 't :> InputElement>
            (
                mapper: IObservable<PointerReleasedEventArgs> -> IObservable<PointerReleasedEventArgs>,
                func: PointerReleasedEventArgs -> unit,
                ?subPatchOptions
            ) =
            AttrBuilder<'t>
                .CreateSubscriptionExt<PointerReleasedEventArgs>(
                    InputElement.PointerReleasedEvent,
                    mapper,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

        static member onPointerMovedExt<'t when 't :> InputElement>
            (
                mapper: IObservable<PointerEventArgs> -> IObservable<PointerEventArgs>,
                func: PointerEventArgs -> unit,
                ?subPatchOptions
            ) =
            AttrBuilder<'t>
                .CreateSubscriptionExt<PointerEventArgs>(
                    InputElement.PointerMovedEvent,
                    mapper,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

module EventLib =
    let handled<'arg when 'arg :> RoutedEventArgs> (args: 'arg) =
        args.Handled <- true
        args

    let getProperties (e: PointerEventArgs) =
        e.GetCurrentPoint(e.Source :?> Avalonia.Visual).Properties

    let filterLeftButton observable =
        Observable.filter (getProperties >> (_.IsLeftButtonPressed)) observable

    let filterRightButton observable =
        Observable.filter (getProperties >> (_.IsRightButtonPressed)) observable

    let filterMiddleButton observable =
        Observable.filter (getProperties >> (_.IsMiddleButtonPressed)) observable
    
    let splitByLeftButton observable =
        observable
        |> Observable.choose (fun e ->
            let props = getProperties e

            if props.IsLeftButtonPressed then Some(Choice1Of2(e))
            else if props.IsRightButtonPressed then Some(Choice2Of2(e))
            else None)
        |> Observable.split id
