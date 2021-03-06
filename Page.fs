namespace Conceal

open System

type Color =
  { A: byte; R: byte; G: byte; B: byte }

type CodeStyles =
  { Background: Color
    FontName: string
    CommentColor: Color
    DefaultColor: Color
    IdentifierColor1: Color
    IdentifierColor2: Color
    InactiveCodeColor: Color
    KeywordColor1: Color
    KeywordColor2: Color
    KeywordColor3: Color
    LineCommentColor: Color
    NumberColor: Color
    OperatorColor: Color
    PreprocessorKeywordColor: Color
    PunctuationColor: Color
    StringColor: Color
    TextColor: Color
    UpperIdentifierColor: Color
    ResultBackground: Color
    ErrorColor: Color }

type Style =
  { TextColor: Color
    LinkColor: Color
    CodeStyles: CodeStyles
    QuoteBackgroundColor: Color
    TitleSizeRate: float
    HeaderSizeRate: float
    TextSizeRate: float }
  member this.TitleSize(height: int) =
    float height * this.TitleSizeRate
  member this.HeaderSize(height: int) =
    float height * this.HeaderSizeRate
  member this.TextSize(height: int) =
    float height * this.TextSizeRate

type TextElement =
  { Value: string
    Link: string option
    Code: bool
    Color: Color }
  static member Create(text: string, link: string option, code: bool, color: Color) =
    { Value = text; Link = link; Code = code; Color = color }
  static member CreateText(text: string, color: Color) =
    { Value = text; Link = None; Code = false; Color = color }
  static member CreateLink(text: string, link: string, color: Color) =
    { Value = text; Link = Some link; Code = false; Color = color }
  static member CreateCode(text: string, color: Color) =
    { Value = text; Link = None; Code = true; Color = color }

type Text =
  { Elements: TextElement list }
  static member Create(elements: TextElement[]) =
    { Elements = elements |> Array.toList }
  static member Create(firstElement: TextElement, [<ParamArray>] elements: TextElement[]) =
    { Elements = firstElement::(elements |> Array.toList) }

type Language =
  { LanguageName: string
    WithRunning: bool }

type ImageBody =
  | Svg of string
  | Png of byte[]
  | Jpeg of byte[]

type PageContent =
  | Text of Text
  | Code of Language * Text list
  | Quote of Text
  | List of PageContent list list
  | Image of ImageBody
  static member CreateText(text: Text) = Text text
  static member CreateCode(lang: Language, lines: Text list) = Code (lang, lines)
  static member CreateQuote(text: Text) = Quote text
  static member CreateList(listItems: PageContent list list) = List listItems
  static member CreateSvg(svgContent: string) = Image (Svg svgContent)
  static member CreatePng(pngContent: byte[]) = Image (Png pngContent)
  static member CreateJpeg(jpegContent: byte[]) = Image (Jpeg jpegContent)

type PageType = TitlePage | ContentPage

type Page =
  { PageType: PageType
    Header: PageContent list
    Body: PageContent list }
  static member Create(pageType: PageType, header: PageContent list, body: PageContent list) =
    { PageType = pageType; Header = header; Body = body }
