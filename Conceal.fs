namespace Conceal

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Controls.Skia
open WebViewControl
open global.Svg.Skia

open WebView
open SKPictureControl

module Conceal =
  type Slides =
    { Pages: Page[]
      Current: int
      Browser: string option }
    member this.Next = { this with Current = min (this.Current + 1) (this.Pages.Length - 1) }
    member this.Prev = { this with Current = max (this.Current - 1) 0 }
    member this.CurrentPage = this.Pages[this.Current]

  type ViewInfo =
    { Width: int
      Height: int
      Style: Style
      Path: string option }

  type ContentInfo =
    { ViewInfo: ViewInfo
      OnLink: bool
      MaxFontSize: float
      MaxImageSize: float }

  type State =
    | Empty
    | WithInputPath of string
    | WithSlides of Slides * OnLink: bool
    static member Load(style: Style, path: string) =
      match SlidesLoader.load style path with
      | Some pages -> WithSlides ({ Pages = pages; Current = 0; Browser = None }, false)
      | None -> Empty

  type Message =
    | Next
    | Prev
    | OnLink
    | OffLink
    | OpenWebPage of string
    | CloseWebPage
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
    | WithSlides (pages, onLink) ->
        match msg with
        | Next -> WithSlides (pages.Next, onLink)
        | Prev -> WithSlides (pages.Prev, onLink)
        | OnLink -> WithSlides (pages, true)
        | OffLink -> WithSlides (pages, false)
        | OpenWebPage link -> WithSlides ({ pages with Browser = Some link }, onLink)
        | CloseWebPage -> WithSlides ({ pages with Browser = None }, onLink)
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

  let toFontFamily (fontName: string) =
    Media.FontFamily(fontName)

  let private normalCursor = new Cursor(StandardCursorType.Arrow)
  let private linkCursor = new Cursor(StandardCursorType.Hand)
  let rec buildContentView (info: ContentInfo) (pageType: PageType) (dispatch: Message -> unit) (content: PageContent) : IView =
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
                TextBlock.fontSize info.MaxFontSize
                TextBlock.foreground (toBrush t.Color)
                TextBlock.text t.Value
                match t.Link with
                | None -> ()
                | Some link ->
                    TextBlock.cursor (if info.OnLink then linkCursor else normalCursor)
                    TextBlock.textDecorations (Media.TextDecorationCollection.Parse("Underline"))
                    TextBlock.onPointerEnter (fun args -> dispatch OnLink)
                    TextBlock.onPointerLeave (fun args -> dispatch OffLink)
                    TextBlock.onTapped (fun args -> dispatch (OpenWebPage link))
              ]
          ]
        ]
    | Code lines ->
       Canvas.create [
         Canvas.width (float info.ViewInfo.Width * 0.9)
         Canvas.children [
           StackPanel.create [
             StackPanel.orientation Orientation.Vertical
             StackPanel.background (toBrush info.ViewInfo.Style.CodeStyles.Background)
             StackPanel.horizontalAlignment HorizontalAlignment.Center
             StackPanel.width (float info.ViewInfo.Width * 0.9)
             StackPanel.children [
               for line in lines do
                 StackPanel.create [
                   StackPanel.orientation Orientation.Horizontal
                   StackPanel.children [
                     for t in line.Elements do 
                       TextBlock.create [
                         TextBlock.fontFamily (toFontFamily info.ViewInfo.Style.CodeStyles.FontName)
                         TextBlock.fontSize (info.MaxFontSize * 0.8)
                         TextBlock.foreground (toBrush t.Color)
                         TextBlock.text t.Value
                       ]
                   ]
                 ]
             ]
           ]
           Button.create [
             Button.right 0.0
             Button.fontSize (info.MaxFontSize * 0.5)
             Button.content "Run"
           ]
         ]
       ]
    | List listItems ->
        StackPanel.create [
          StackPanel.orientation Orientation.Vertical
          StackPanel.children [
            for listItem in listItems do
              StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                  TextBlock.create [
                    TextBlock.verticalAlignment VerticalAlignment.Top
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.fontSize info.MaxFontSize
                    TextBlock.text "・ "
                  ]
                  StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children [
                      for elem in listItem do
                        buildContentView info pageType dispatch elem
                    ]
                  ]
                ]
              ]
          ]
        ]
    | Image (Svg svgContent) ->
        let svg = new SKSvg()
        let pict = svg.FromSvg(svgContent)
        SKPictureControl.create [
          SKPictureControl.height info.MaxImageSize
          //SKPictureControl.stretch Media.Stretch.None
          SKPictureControl.picture pict
        ]

  let buildContentsView (info: ContentInfo) (pageType: PageType) (contents: PageContent list) (dispatch: Message -> unit) =
    contents |> List.map (buildContentView info pageType dispatch)

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

  let buildBrowserView info link dispatch =
    DockPanel.create [
      DockPanel.children [
        TextBlock.create [
          TextBlock.dock Dock.Top
          TextBlock.fontSize (float info.Height * 0.03)
          TextBlock.text "Esc: returns presentation"
        ]
        WebViewControl.WebView.create [
          WebView.address link
          WebView.onKeyDown (fun args ->
            if args.Key = Avalonia.Input.Key.Escape then
              args.Handled <- true
              dispatch CloseWebPage
          )
        ]
      ]
    ]

  let rec private textLines (contents: PageContent list) =
    match contents with
    | [] -> 0
    | (Image _)::rest -> textLines rest
    | (List items)::rest -> (items |> List.sumBy textLines) + textLines rest
    | (Text _)::rest -> 1 + textLines rest
    | (Code lines)::rest -> List.length lines + textLines rest

  let view (info: ViewInfo) (state: State) dispatch =
    match state with
    | Empty ->
        buildLoadPageView "" dispatch
    | WithInputPath path ->
        buildLoadPageView path dispatch
    | WithSlides ({ Browser = Some link; Pages = _; Current = _ }, _) ->
        buildBrowserView info link dispatch
    | WithSlides (pages, onLink) ->
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
                let headerInfo =
                  let maxSize =
                    match crntPage.PageType with
                    | TitlePage -> info.Style.TitleSize(info.Height)
                    | ContentPage -> info.Style.HeaderSize(info.Height)
                  { ViewInfo = info; OnLink = onLink; MaxFontSize = maxSize; MaxImageSize = maxSize }
                let maxHeaderHeight = headerInfo.MaxFontSize * (float (List.length crntPage.Header))
                let maxBodyHeight = float info.Height - maxHeaderHeight
                // TODO : treat multiple images
                let imageHeight =
                  maxBodyHeight - (info.Style.TextSize(info.Height) * (crntPage.Body |> textLines |> (+)1 |> float))
                  |> (*)0.9
                let bodyInfo =
                  { ViewInfo = info; OnLink = onLink; MaxFontSize = info.Style.TextSize(info.Height); MaxImageSize = imageHeight }
                yield! buildContentsView headerInfo crntPage.PageType crntPage.Header dispatch
                yield! buildContentsView bodyInfo crntPage.PageType crntPage.Body dispatch
              ]
            ]
          ]
        ]
