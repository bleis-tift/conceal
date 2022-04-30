namespace Conceal

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Elmish

module FileSelector =
  type State =
    { InputFilePath: string
      StartPresentation: bool }

  type Msg =
    | UpdatePath of string
    | StartPresentation

  let init () =
    { InputFilePath = ""
      StartPresentation = false }

  let update (msg: Msg) (state: State) =
    match msg with
    | UpdatePath path -> { state with InputFilePath = path }, Cmd.none
    | StartPresentation -> { state with StartPresentation = true }, Cmd.none

  let view (state: State) dispatch =
    DockPanel.create [
      DockPanel.lastChildFill false
      DockPanel.children [
        DockPanel.create [
          DockPanel.dock Dock.Top
          DockPanel.children [
            Button.create [
              Button.dock Dock.Right
              Button.content "Start Presentation"
              Button.onClick (fun args ->
                args.Handled <- true
                dispatch StartPresentation
              )
            ]
            TextBox.create [
              TextBox.text state.InputFilePath
              TextBox.onTextChanged (UpdatePath >> dispatch)
            ]
          ]
        ]
      ]
    ]

