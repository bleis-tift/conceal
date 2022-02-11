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
    | WithInputPath of string
    | WithSlides of Slides
    static member Load(style: Style, path: string) =
      match SlidesLoader.load style path with
      | Some pages -> WithSlides { Pages = pages; Current = 0 }
      | None -> Empty

  type Message =
    | Next
    | Prev
    | UpdatePath of string
    | StartPresentation

  let update (style: Style) (msg: Message) (state: State) : State =
    match state with
    | Empty ->
        match msg with
        | UpdatePath "" -> Empty
        | UpdatePath path -> WithInputPath path
        | other -> eprintfn "Received an invalid message: %A (current state: Emppty)" other; state
    | WithInputPath path ->
        match msg with
        | StartPresentation -> State.Load(style, path)
        | UpdatePath path -> WithInputPath path
        | other -> eprintfn "Received an invalid message: %A (current state: WithInputPath)" other; state
    | WithSlides pages ->
        match msg with
        | Next -> WithSlides pages.Next
        | Prev -> WithSlides pages.Prev
        | other -> eprintfn "Received an invalid message: %A (current state: WithSlides)" other; state

  // for-debug
  let private withBorder (v: IView) =
    Border.create [
      Border.borderThickness 1.0
      Border.borderBrush "#ffff0000"
      Border.child v
    ]

  let toAvaloniaColor (color: Color) =
    Media.Color.FromArgb(color.A, color.R, color.G, color.B)

  let toBrush (color: Color) =
    Media.SolidColorBrush(toAvaloniaColor color)

  let buildContentView (pageType: PageType) (content: PageContent) : IView =
    match content with
    | Text text ->
        StackPanel.create [
          StackPanel.orientation Orientation.Horizontal
          StackPanel.verticalAlignment VerticalAlignment.Center
          if pageType = TitlePage then
            StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.children [
            for t in text.Elements do
              TextBlock.create [
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.fontSize 48.0
                TextBlock.foreground (toBrush t.Color)
                TextBlock.text t.Value
              ]
          ]
        ]

  let buildContentsView (pageType: PageType) (contents: PageContent list) =
    contents |> List.map (buildContentView pageType)

  let buildLoadPageView path dispatch =
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
              TextBox.text path
              TextBox.onTextChanged (UpdatePath >> dispatch)
            ]
          ]
        ]
      ]
    ]

  let view (state: State) dispatch =
    match state with
    | Empty ->
        buildLoadPageView "" dispatch
    | WithInputPath path ->
        buildLoadPageView path dispatch
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
              if crntPage.PageType = TitlePage then
                StackPanel.verticalAlignment VerticalAlignment.Center
              StackPanel.children [
                yield! buildContentsView crntPage.PageType crntPage.Header
                yield! buildContentsView crntPage.PageType crntPage.Body
              ]
            ]
          ]
        ]
