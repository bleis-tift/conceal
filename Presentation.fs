namespace Conceal

open System.IO
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Controls.Skia
open Elmish
open global.Svg.Skia

open SKPictureControl

module Presentation =
  type State =
    { Pages: Page[]
      CurrentPageIndex: int
      OnLink: bool
      Browser: string option
      RunResult: Deferred<CodeRunner.RunResult> }
    member this.Next =
      { this with CurrentPageIndex = min (this.CurrentPageIndex + 1) (this.Pages.Length - 1) }
    member this.Prev =
      { this with CurrentPageIndex = max (this.CurrentPageIndex - 1) 0 }
    member this.CurrentPage =
      this.Pages[this.CurrentPageIndex]

  type Msg =
    | Next
    | Prev
    | OnLink
    | OffLink
    | OpenBrowser of string
    | RunCode of Language * string * AsyncOperationStatus<CodeRunner.RunResult>
    | CloseRunResult

  let init pages =
    { Pages = pages; CurrentPageIndex = 0; OnLink = false; Browser = None; RunResult = HasNotStartedYet }

  let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | Next ->
        state.Next, Cmd.none
    | Prev ->
        state.Prev, Cmd.none
    | OnLink ->
        { state with OnLink = true }, Cmd.none
    | OffLink ->
        { state with OnLink = false }, Cmd.none
    | OpenBrowser url ->
        { state with Browser = Some url }, Cmd.none
    | RunCode (lang, code, Started) ->
        let runAsync (lang, code: string) = async {
          let! result = CodeRunner.runAsync "dotnet" code
          return (lang, code, Finished result)
        }
        let cmd = Cmd.OfAsync.perform runAsync (lang, code) RunCode
        { state with RunResult = InProgress }, cmd
    | RunCode (lang, code, Finished runResult) ->
        { state with RunResult = Resolved runResult }, Cmd.none
    | CloseRunResult ->
        { state with RunResult = HasNotStartedYet }, Cmd.none

  type ContentInfo =
    { AppInfo: AppInfo
      OnLink: bool
      InQuote: bool
      MaxFontSize: float
      MaxImageSize: float }

  let rec private textLines (contents: PageContent list) =
    match contents with
    | [] -> 0
    | (Image _)::rest -> textLines rest
    | (List items)::rest -> (items |> List.sumBy textLines) + textLines rest
    | (Text _)::rest -> 1 + textLines rest
    | (Code (_, lines))::rest -> List.length lines + textLines rest
    | (Quote _)::rest -> 1 + textLines rest

  let private contentInfos info onLink currentPage =
    let headerInfo =
      let maxSize =
        match currentPage.PageType with
        | TitlePage -> info.Style.TitleSize(info.Height)
        | ContentPage -> info.Style.HeaderSize(info.Height)
      { AppInfo = info; OnLink = onLink; InQuote = false; MaxFontSize = maxSize; MaxImageSize = maxSize }
    let maxHeaderHeight = headerInfo.MaxFontSize * (float (List.length currentPage.Header))
    let maxBodyHeight = float info.Height - maxHeaderHeight
    let maxFontSize = info.Style.TextSize(info.Height)
    // TODO : 複数イメージの扱い
    let imageHeight =
      maxBodyHeight - (maxFontSize * (currentPage.Body |> textLines |> (+)1 |> float))
      |> (*)0.9
    let bodyInfo =
      { AppInfo = info; OnLink = onLink; InQuote = false; MaxFontSize = maxFontSize; MaxImageSize = imageHeight }
    (headerInfo, bodyInfo)

  let private code (lines: Text list) =
    lines
    |> List.map (fun x -> x.Elements |> List.map (fun e -> e.Value) |> String.concat "")
    |> String.concat "\r\n"

  let private normalCursor = new Cursor(StandardCursorType.Arrow)
  let private linkCursor = new Cursor(StandardCursorType.Hand)

  let rec private viewContent (info: ContentInfo) (pageType: PageType) dispatch (content: PageContent) =
    let viewText (text: Text) : IView =
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
              if info.InQuote then
                TextBlock.fontStyle FontStyle.Italic
              if t.Code then
                TextBlock.fontFamily (AvaloniaObject.fontFamilyFrom info.AppInfo.Style.CodeStyles.FontName)
              TextBlock.foreground (AvaloniaObject.brushFrom t.Color)
              TextBlock.text t.Value
              match t.Link with
              | None -> ()
              | Some link ->
                  TextBlock.cursor (if info.OnLink then linkCursor else normalCursor)
                  TextBlock.textDecorations (TextDecorationCollection.Parse("Underline"))
                  TextBlock.onPointerEnter (fun _ -> dispatch OnLink)
                  TextBlock.onPointerLeave (fun _ -> dispatch OffLink)
                  TextBlock.onTapped (fun _ -> dispatch (OpenBrowser link))
            ]
        ]
      ]
    let viewCode (lang: Language) (lines: Text list) =
      Canvas.create [
        Canvas.width (float info.AppInfo.Width * 0.9)
        Canvas.children [
          StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.background (AvaloniaObject.brushFrom info.AppInfo.Style.CodeStyles.Background)
            StackPanel.horizontalAlignment HorizontalAlignment.Center
            StackPanel.width (float info.AppInfo.Width * 0.9)
            StackPanel.children [
              let fontSize =
                info.MaxFontSize * min 0.8 (7.8 / float (List.length lines))
              for line in lines do
                StackPanel.create [
                  StackPanel.orientation Orientation.Horizontal
                  StackPanel.children [
                    for t in line.Elements do
                      TextBlock.create [
                        TextBlock.fontFamily (AvaloniaObject.fontFamilyFrom info.AppInfo.Style.CodeStyles.FontName)
                        TextBlock.fontSize fontSize
                        TextBlock.foreground (AvaloniaObject.brushFrom t.Color)
                        TextBlock.text t.Value
                      ]
                  ]
                ]
            ]
          ]
          if lang.WithRunning then
            let c = code lines
            Button.create [
              Button.right 0.0
              Button.fontSize (info.MaxFontSize * 0.5)
              Button.content "Run"
              Button.onClick (fun args ->
                args.Handled <- true
                dispatch (RunCode (lang, c, Started))
              )
            ]
        ]
      ]
    let viewQuote (text: Text) =
      StackPanel.create [
        StackPanel.background (AvaloniaObject.brushFrom info.AppInfo.Style.QuoteBackgroundColor)
        StackPanel.width (float info.AppInfo.Width * 0.9)
        StackPanel.children [
          viewContent { info with InQuote = true } pageType dispatch (Text text)
        ]
      ]
    let viewList (listItems: PageContent list list) =
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
                      viewContent info pageType dispatch elem
                  ]
                ]
              ]
            ]
        ]
      ]
    let viewSvg (svgContent: string) =
      let svg = new SKSvg()
      let pict = svg.FromSvg(svgContent)
      SKPictureControl.create [
        SKPictureControl.height info.MaxImageSize
        SKPictureControl.picture pict
      ]
    let viewBitmap (bs: byte[]) =
      let bitmap = new Bitmap(new MemoryStream(bs))
      Image.create [
        Image.height info.MaxImageSize
        Image.source bitmap
      ]

    match content with
    | Text text -> viewText text
    | Code (lang, lines) -> viewCode lang lines
    | Quote text -> viewQuote text
    | List listItems -> viewList listItems
    | Image (Svg svgContent) -> viewSvg svgContent
    | Image (Png content|Jpeg content) -> viewBitmap content

  let private viewContents (info: ContentInfo) (pageType: PageType) (contents: PageContent list) dispatch =
    contents |> List.map (viewContent info pageType dispatch)

  let private viewCurrentPage (info: AppInfo) (onLink: bool) (currentPage: Page) dispatch =
    StackPanel.create [
      StackPanel.orientation Orientation.Vertical
      if currentPage.PageType = TitlePage then
        StackPanel.verticalAlignment VerticalAlignment.Center
      StackPanel.children [
        let headerInfo, bodyInfo = contentInfos info onLink currentPage
        yield! viewContents headerInfo currentPage.PageType currentPage.Header dispatch
        yield! viewContents bodyInfo currentPage.PageType currentPage.Body dispatch
      ]
    ]

  let private splitLines chunkSize (output: string) =
    output
    |> Seq.chunkBySize chunkSize
    |> Seq.map (fun cs -> System.String.Join("", cs))
    |> Seq.toList

  let private viewOverlay (info: AppInfo) (state: State) dispatch overlay =
    DockPanel.children [
      Grid.create [
        Grid.children [
          let headerInfo, bodyInfo = contentInfos info state.OnLink state.CurrentPage
          viewCurrentPage info false state.CurrentPage dispatch
          Canvas.create [
            Canvas.height (float info.Height - headerInfo.MaxFontSize * 0.9)
            Canvas.width (float info.Width * 0.9)
            Canvas.background (AvaloniaObject.brushFrom info.Style.CodeStyles.ResultBackground)
            Canvas.children (overlay bodyInfo)
          ]
        ]
      ]
    ]

  let view (info: AppInfo) (state: State) dispatch =
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
      match state.RunResult with
      | HasNotStartedYet ->
          DockPanel.children [
            viewCurrentPage info state.OnLink state.CurrentPage dispatch
          ]
      | InProgress ->
          viewOverlay info state dispatch (fun bodyInfo -> [
            TextBlock.create [
              TextBlock.fontSize bodyInfo.MaxFontSize
              TextBlock.foreground (AvaloniaObject.brushFrom info.Style.TextColor)
              TextBlock.text "Running..."
            ]
          ])
      | Resolved res ->
          viewOverlay info state dispatch (fun bodyInfo -> [
            Button.create [
              Button.right 0.0
              Button.fontSize (bodyInfo.MaxFontSize * 0.5)
              Button.content "Close"
              Button.onClick (fun args ->
                args.Handled <- true
                dispatch CloseRunResult
              )
            ]
            StackPanel.create [
              StackPanel.orientation Orientation.Vertical
              StackPanel.children [
                for line in splitLines 50 res.Output do
                  TextBlock.create [
                    if res.Error then
                      TextBlock.foreground (AvaloniaObject.brushFrom info.Style.CodeStyles.ErrorColor)
                    TextBlock.fontFamily (AvaloniaObject.fontFamilyFrom info.Style.CodeStyles.FontName)
                    TextBlock.fontSize (bodyInfo.MaxFontSize * 0.6)
                    TextBlock.text line
                  ]
              ]
            ]
          ])
    ]
