namespace Conceal

open System
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts

type MainWindow(args: string[]) as this =
  inherit HostWindow()
  do
    let allFonts = Media.FontManager.Current.GetInstalledFontFamilyNames() |> Seq.toList
    let codeFonts = ["Consolas"; "MeiryoKe_UIGothic"; "Meiryo UI"; "Yu Gothic UI"]
    let codeFontName = codeFonts |> List.find (fun f -> allFonts |> List.contains f)
    let args = AppInfo.parse codeFontName args
    base.Title <- "Conceal"
    base.Width <- float args.Width
    base.Height <- float args.Height
    base.CanResize <- false
    base.WindowStartupLocation <- WindowStartupLocation.CenterScreen
    
    //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
    //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

    let state = Conceal.init args

    Elmish.Program.mkProgram
      (fun () -> state, Cmd.none)
      (Conceal.update)
      (Conceal.view args)
    |> Program.withHost this
    |> Program.run

type App() =
  inherit Application()

  override this.Initialize() =
    this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
    this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

  override this.OnFrameworkInitializationCompleted() =
    match this.ApplicationLifetime with
    | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
      desktopLifetime.MainWindow <- MainWindow(desktopLifetime.Args)
    | _ -> ()

module Program =

  [<EntryPoint>]
  let main(args: string[]) =
    AppBuilder
      .Configure<App>()
      .UsePlatformDetect()
      .UseSkia()
      .StartWithClassicDesktopLifetime(args)