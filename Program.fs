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

type Args =
  { Width: int
    Height: int
    Path: string option }

module Args =
  let (|Integer|_|) (str: string) =
    match Int32.TryParse(str) with
    | true, result -> Some result
    | false, _ -> None

  let parse args =
    let rec parse' acc = function
    | (Integer w)::(Integer h)::rest -> parse' { acc with Width = w; Height = h } rest
    | path::rest -> parse' { acc with Path = Some path } rest
    | [] -> acc

    parse' { Width = 1600; Height = 900; Path = None } (args |> Array.toList)

type MainWindow(args: string[]) as this =
  inherit HostWindow()
  do
    let args = Args.parse args
    base.Title <- "Conceal"
    base.Width <- float args.Width
    base.Height <- float args.Height
    base.CanResize <- false
    base.WindowStartupLocation <- WindowStartupLocation.CenterScreen
    
    //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
    //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

    Elmish.Program.mkSimple (fun () -> ()) Conceal.update Conceal.view
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