namespace Conceal

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout

module Conceal =
  type Slides =
    { Pages: Page[]
      Current: int }
    member this.Next = { this with Current = min (this.Current + 1) (this.Pages.Length - 1) }
    member this.Prev = { this with Current = max (this.Current - 1) 0 }
    member this.CurrentPage = this.Pages[this.Current]

  type State =
    | Empty
    | WithSlides of Slides
    static member Load(style: Style, path: string) =
      match SlidesLoader.load style path with
      | Some pages -> WithSlides { Pages = pages; Current = 0 }
      | None -> Empty

  type Message =
    | Next
    | Prev

  let update (msg: Message) (state: State) : State =
    match state with
    | Empty ->
        // TODO : impl
        state
    | WithSlides pages ->
        match msg with
        | Next -> WithSlides pages.Next
        | Prev -> WithSlides pages.Prev

  let toAvaloniaColor (color: Color) =
    Media.Color.FromArgb(color.A, color.R, color.G, color.B)

  let toBrush (color: Color) =
    Media.SolidColorBrush(toAvaloniaColor color)

  let buildContentView (content: PageContent) : IView =
    match content with
    | Text text ->
        StackPanel.create [
          StackPanel.orientation Orientation.Horizontal
          StackPanel.children [
            for t in text.Elements do
              TextBlock.create [
                TextBlock.fontSize 48.0
                TextBlock.foreground (toBrush t.Color)
                TextBlock.text t.Value
              ]
          ]
        ]

  let buildContentsView (contents: PageContent list) =
    contents |> List.map buildContentView

  let view (state: State) dispatch =
    match state with
    | Empty ->
        // TODO : impl
        DockPanel.create []
    | WithSlides pages ->
        let crntPage = pages.CurrentPage
        DockPanel.create [
          DockPanel.focusable true
          DockPanel.onKeyDown (fun args ->
            match args.Key with
            | Key.Enter
            | Key.Right
            | Key.Down ->
                args.Handled <- true
                dispatch Next
            | Key.Left
            | Key.Up ->
                args.Handled <- true
                dispatch Prev
            | _ -> ()
          )
          DockPanel.children [
            StackPanel.create [
              StackPanel.orientation Orientation.Vertical
              StackPanel.children [
                yield! buildContentsView crntPage.Header
                yield! buildContentsView crntPage.Body
              ]
            ]
          ]
        ]
