namespace Conceal

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Input
open Elmish
open WebViewControl

open WebView

module Browser =
  type State =
    { Url: string
      Close: bool }

  type Msg =
    | CloseBrowser

  let init url =
    { Url = url; Close = false }

  let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | CloseBrowser ->
        { state with Close = true }, Cmd.none

  let view (info: AppInfo) (state: State) dispatch =
    DockPanel.create [
      DockPanel.children [
        TextBlock.create [
          TextBlock.dock Dock.Top
          TextBlock.fontSize (float info.Height * 0.03)
          TextBlock.text "Esc: returns presentation"
        ]
        WebView.create [
          WebView.address state.Url
          WebView.onKeyDown (fun args ->
            if args.Key = Key.Escape then
              args.Handled <- true
              dispatch CloseBrowser
          )
        ]
      ]
    ]

