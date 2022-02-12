namespace Conceal

open System

type Color =
  { A: byte; R: byte; G: byte; B: byte }

type Style =
  { TextColor: Color
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
    Color: Color }
  static member Create(text: string, color: Color) =
    { Value = text; Color = color }

type Text =
  { Elements: TextElement list }
  static member Create(firstElement: TextElement, [<ParamArray>] elements: TextElement[]) =
    { Elements = firstElement::(elements |> Array.toList) }

type ImageBody =
  | Svg of string

type PageContent =
  | Text of Text
  | List of PageContent list list
  | Image of ImageBody
  static member CreateText(text: Text) = Text text
  static member CreateList(listItems: PageContent list list) = List listItems
  static member CreateSvg(svgContent: string) = Image (Svg svgContent)

type PageType = TitlePage | ContentPage

type Page =
  { PageType: PageType
    Header: PageContent list
    Body: PageContent list }
  static member Create(pageType: PageType, header: PageContent list, body: PageContent list) =
    { PageType = pageType; Header = header; Body = body }
