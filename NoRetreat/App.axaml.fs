namespace NoRetreat

open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Data.Core.Plugins
open Avalonia.Themes.Fluent
open Avalonia.Markup.Xaml
open Elmish
open Avalonia.FuncUI
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish




open NoRetreat.Game

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "Counter Example"
        //base.Icon <- WindowIcon(System.IO.Path.Combine("Assets", "avalonia-logo.ico"))
        base.Height <- 700.0
        base.Width <- 700.0

        #if DEBUG
        base.AttachDevTools()
        //TopLevel.GetTopLevel(this).RendererDiagnostics.DebugOverlays <- Avalonia.Rendering.RendererDebugOverlays.Fps
        #endif

        Elmish.Program.mkProgram Main.init Main.update Main.view
        |> Program.withHost this
        |> Program.withErrorHandler (fun (s,e) ->
            MsBox.Avalonia.MessageBoxManager
                .GetMessageBoxStandard("Error", s + "\n--------------\n" + e.Message)
                .ShowWindowAsync()
            |> Async.AwaitTask |> ignore)
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        AvaloniaXamlLoader.Load(this)
        this.Styles.Add (FluentTheme())
        this.Styles.Load "avares://NoRetreat/Styles/CardHandStyle.axaml"

    override this.OnFrameworkInitializationCompleted() =
        BindingPlugins.DataValidators.RemoveAt(0)

        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()
