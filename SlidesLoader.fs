namespace Conceal

open System
open System.IO
open System.Net.Http
open System.Threading.Tasks
open FSharp.Compiler.Tokenization
open FSharp.Data
open FSharp.Formatting.Markdown

module SlidesLoader =
  let rec private (|SpanText|) (spans: MarkdownSpans) =
    spans
    |> List.map (function
                 | Literal (lit, _) -> lit
                 | InlineCode (code, _) -> code
                 | DirectLink (SpanText body, _, _, _) -> body
                 | unsupported -> failwithf "unsupported: %A" unsupported)
    |> String.concat ""

  let private split (pred: 'a -> bool) xs =
    let rec loop result temp = function
    | [] -> (temp |> List.rev)::result |> List.rev
    | x::xs when pred x -> loop ((temp |> List.rev)::result) [] xs
    | x::xs -> loop result (x::temp) xs
    
    loop [] [] xs

  let private toHeader (style: Style) (paragraphs: MarkdownParagraphs) =
    match paragraphs with
    | (Heading (n, SpanText text, _))::rest ->
        let content = PageContent.CreateText(Text.Create(TextElement.CreateText(text, style.TextColor)))
        [content], (if n = 1 then TitlePage else ContentPage), rest
    | other -> [], ContentPage, other

  let private client = new HttpClient()

  let private loadImage (httpLoader: string -> Task<'a>) (fileLoader: string -> 'a) (src: string) =
    if src.StartsWith("http") then
      (httpLoader src).Result
    else
      fileLoader src

  let private toImage (imageNode: HtmlNode) =
    let src = imageNode.AttributeValue("src")
    match Path.GetExtension(src) with
    | ".svg" ->
        let content = loadImage client.GetStringAsync File.ReadAllText src
        PageContent.CreateSvg(content)
    | ".png" ->
        let content = loadImage client.GetByteArrayAsync File.ReadAllBytes src
        PageContent.CreatePng(content)
    | unsupported -> failwithf "unsupported image file: %A" unsupported

  let private toImages (node: HtmlNode) =
    match node.Elements("img") with
    | [] -> failwith "Empty html node"
    | images -> images |> List.map toImage

  let private toTextElement (style: Style) (span: MarkdownSpan) =
    match span with
    | DirectLink(SpanText body, link, title, _) ->
        TextElement.CreateLink(body, link, style.LinkColor)
    | Literal(text, _) ->
        TextElement.CreateText(text, style.TextColor)
    | InlineCode(code, _) ->
        TextElement.CreateCode(code, style.TextColor)
    | unsupported ->
        failwithf "unsupported span: %A" unsupported

  let private tokenizeLine (line: string) (tokenizer: FSharpLineTokenizer) state =
    let toks = ResizeArray()
    let mutable exit = false
    let mutable state = state
    while not exit do
      match tokenizer.ScanToken(state) with
      | Some tok, newState ->
          toks.Add((line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1), tok))
          state <- newState
      | None, newState ->
          state <- newState
          exit <- true
    toks :> seq<_>, state

  let private sourceTok = FSharpSourceTokenizer([], Some "code.fsx")
  let rec private tokenizeLines state lines =
    lines
    |> Seq.fold (fun (result, st) line ->
         let tokenizer = sourceTok.CreateLineTokenizer(line)
         let toks, newState = tokenizeLine line tokenizer st
         (Seq.append result [toks], newState)) (Seq.empty, state)
    |> fst

  // TODO : consider context
  let private chooseColor (style: CodeStyles) (info: FSharpTokenInfo) =
    match info.ColorClass with
    | FSharpTokenColorKind.Default -> style.DefaultColor
    | FSharpTokenColorKind.Keyword -> style.KeywordColor1
    | FSharpTokenColorKind.Comment -> style.CommentColor
    | FSharpTokenColorKind.Identifier -> style.IdentifierColor1
    | FSharpTokenColorKind.String -> style.StringColor
    | FSharpTokenColorKind.UpperIdentifier -> style.UpperIdentifierColor
    | FSharpTokenColorKind.InactiveCode -> style.InactiveCodeColor
    | FSharpTokenColorKind.PreprocessorKeyword -> style.PreprocessorKeywordColor
    | FSharpTokenColorKind.Number -> style.NumberColor
    | FSharpTokenColorKind.Operator -> style.OperatorColor
    | FSharpTokenColorKind.Punctuation -> style.PunctuationColor

  let colorize (style: Style) (lang: Language) (code: string) =
    let lines = code.Split([|"\r\n"; "\n"|], StringSplitOptions.None)
    let linesToks = tokenizeLines FSharpTokenizerLexState.Initial lines
    linesToks
    |> Seq.map (fun lineToks ->
         let elements =
           lineToks
           |> Seq.map (fun (tok, info) ->
                TextElement.CreateText(tok, chooseColor style.CodeStyles info)
              )
           |> Seq.toArray
         Text.Create(elements)
       )
    |> Seq.toList

  let rec private toBody (style: Style) (paragraph: MarkdownParagraph) =
    match paragraph with
    | Span(spans, _)
    | Paragraph(spans, _) ->
        [PageContent.CreateText(Text.Create(spans |> Seq.map (toTextElement style) |> Seq.toArray))]
    | InlineHtmlBlock(html, ec, _) ->
        let nodes = HtmlNode.Parse(html)
        nodes |> List.collect toImages
    | ListBlock(Unordered, items, _) ->
        let listItems =
          items
          |> List.map (fun item -> item |> List.collect (toBody style))
        [PageContent.CreateList(listItems)]
    | CodeBlock(code, ec, fence, language, _, _) ->
        let lang =
          match language.Split('-') with
          | [|langName|] -> { LanguageName = langName; WithRunning = true }
          | [|langName; "without"; "running"|] -> { LanguageName = langName; WithRunning = false }
          | _ -> failwithf "unsupported language: %A" language
        let linesText = colorize style lang code
        [PageContent.CreateCode(lang, linesText)]
    | QuotedBlock(paragraphs, _) ->
        paragraphs
        |> Seq.map (function
                    | (Paragraph(spans, _)) ->
                        let text = Text.Create(spans |> Seq.map (toTextElement style) |> Seq.toArray)
                        PageContent.CreateQuote(text)
                    | unsupported ->
                        failwithf "unsupported quote style. paragraph=%A" unsupported)
        |> Seq.toList
    | Heading _ -> failwith "unexpected heading. maybe you forgot page separation(--- or --)."
    | unsupported -> failwithf "unsupported paragraph. paragraph=%A" unsupported

  let private toPage (style: Style) (paragraphs: MarkdownParagraphs) : Page =
    let header, pageType, rest = toHeader style paragraphs
    let body = rest |> Seq.collect (toBody style) |> Seq.toList
    Page.Create(pageType, header, body)

  let private toSlide (style: Style) (paragraphs: MarkdownParagraphs) =
    paragraphs
    |> split (function Paragraph (SpanText "--", _) -> true | _ -> false)
    |> List.map (toPage style)

  let private toSlides (style: Style) (paragraphs: MarkdownParagraphs) =
    paragraphs
    |> split (function
              | HorizontalRule ('-', Some range) -> range.EndColumn - range.StartColumn = 3
              | _ -> false)
    |> List.collect (toSlide style)

  let private convertFrom (style: Style) (markdown: MarkdownDocument) =
    Some (toSlides style markdown.Paragraphs |> Seq.toArray)

  let load (style: Style) (path: string) =
    try
      let document = File.ReadAllText(path)
      let parsed = Markdown.Parse(document)
      convertFrom style parsed
    with
      e ->
        printfn "%A" e
        None
