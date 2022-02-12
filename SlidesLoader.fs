namespace Conceal

open System
open System.IO
open System.Net.Http
open FSharp.Data
open FSharp.Formatting.Markdown

module SlidesLoader =
  let private (|SpanText|) (spans: MarkdownSpans) =
    spans
    |> List.map (function Literal (lit, _) -> lit)
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

  let private toImage (imageNode: HtmlNode) =
    let src = imageNode.AttributeValue("src")
    let content = client.GetStringAsync(src).Result
    PageContent.CreateSvg(content)

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
        // TODO : colorize
        let lines =
          code.Split([|"\r\n"; "\n"|], StringSplitOptions.None)
          |> Array.map (fun line -> Text.Create(TextElement.CreateText(line, style.TextColor)))
          |> Array.toList
        [PageContent.CreateCode(lines)]
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
