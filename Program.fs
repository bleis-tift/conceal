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
    Style: Style
    Path: string option }

module Args =
  let (|Integer|_|) (str: string) =
    match Int32.TryParse(str) with
    | true, result -> Some result
    | false, _ -> None

  let (|OptionName|_|) (prefix: string) (str: string) =
    if str.StartsWith(prefix) then
      Some (str.Substring(prefix.Length))
    else
      None

  let parse args =
    let rec parse' acc = function
    | (Integer w)::(Integer h)::rest -> parse' { acc with Width = w; Height = h } rest
    | (OptionName "style=" "dark")::rest -> parse' { acc with Style = Styles.dark } rest
    | path::rest -> parse' { acc with Path = Some path } rest
    | [] -> acc

    parse' { Width = 1600; Height = 900; Style = Styles.dark; Path = None } (args |> Array.toList)

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

    let state =
      match args.Path with
      | Some path -> Conceal.State.Load(args.Style, path)
      | None -> Conceal.State.Empty

    Elmish.Program.mkSimple (fun () -> state) (Conceal.update args.Style) Conceal.view
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