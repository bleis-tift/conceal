namespace Conceal

open System

type Color =
  { A: byte; R: byte; G: byte; B: byte }

type Style =
  { TextColor: Color }

type TextElement =
  { Value: string
    Color: Color }
  static member Create(text: string, color: Color) =
    { Value = text; Color = color }

type Text =
  { Elements: TextElement list }
  static member Create(firstElement: TextElement, [<ParamArray>] elements: TextElement[]) =
    { Elements = firstElement::(elements |> Array.toList) }

type PageContent =
  | Text of Text
  static member CreateText(text: Text) = Text text

type PageType = TitlePage | ContentPage

type Page =
  { PageType: PageType
    Header: PageContent list
    Body: PageContent list }
  static member Create(pageType: PageType, header: PageContent list, body: PageContent list) =
    { PageType = pageType; Header = header; Body = body }
