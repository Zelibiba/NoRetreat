namespace NoRetreat

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Data.Core.Plugins
open Avalonia.Themes.Fluent
open Avalonia.Markup.Xaml
open Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish

open NoRetreat.Model

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "Counter Example"
        //base.Icon <- WindowIcon(System.IO.Path.Combine("Assets", "avalonia-logo.ico"))
        base.Height <- 400.0
        base.Width <- 400.0

        #if DEBUG
        base.AttachDevTools()
        #endif

        Elmish.Program.mkSimple Cell.init Cell.update Cell.view
        |> Program.withHost this
        |> Program.withConsoleTrace
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        AvaloniaXamlLoader.Load(this)
        this.Styles.Add (FluentTheme())

    override this.OnFrameworkInitializationCompleted() =
        BindingPlugins.DataValidators.RemoveAt(0)

        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()
