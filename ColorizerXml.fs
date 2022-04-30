namespace Conceal

open System.Xml.Linq

module ColorizerXml =
  let rec colorize indent (style: Style) (elem: XElement) =
    let tagName = elem.Name.LocalName
    let attrs = elem.Attributes()
    let children = elem.Nodes()
    let firstLine =
      Text.Create(
        if children |> Seq.isEmpty then
          [| TextElement.CreateText(indent + "<", style.CodeStyles.InactiveCodeColor)
             TextElement.CreateText(tagName, style.CodeStyles.KeywordColor1)
             for attr in attrs do
               TextElement.CreateText(" " + attr.Name.LocalName, style.CodeStyles.IdentifierColor1)
               TextElement.CreateText("=", style.CodeStyles.DefaultColor)
               TextElement.CreateText("\"" + attr.Value + "\"", style.CodeStyles.StringColor)
             TextElement.CreateText("/>", style.CodeStyles.InactiveCodeColor) |]
        else
          [| TextElement.CreateText(indent + "<", style.CodeStyles.InactiveCodeColor)
             TextElement.CreateText(tagName, style.CodeStyles.KeywordColor1)
             for attr in attrs do
               TextElement.CreateText(" " + attr.Name.LocalName, style.CodeStyles.IdentifierColor1)
               TextElement.CreateText("=", style.CodeStyles.DefaultColor)
               TextElement.CreateText("\"" + attr.Value + "\"", style.CodeStyles.StringColor)
             TextElement.CreateText(">", style.CodeStyles.InactiveCodeColor) |])
    let lastLine =
      if children |> Seq.isEmpty then []
      else
        [ Text.Create([|
            TextElement.CreateText(indent + "</", style.CodeStyles.InactiveCodeColor)
            TextElement.CreateText(tagName, style.CodeStyles.KeywordColor1)
            TextElement.CreateText(">", style.CodeStyles.InactiveCodeColor)
          |])]
    [ yield firstLine
      for child in children do
        match child with
        | :? XElement as elem -> yield! colorize (indent + " ") style elem
        | :? XText as text -> yield Text.Create(TextElement.CreateText(indent + " " + text.Value, style.CodeStyles.DefaultColor))
        | :? XComment as comment ->
            yield Text.Create(TextElement.CreateText(indent + " <!-- " + comment.Value + " -->", style.CodeStyles.CommentColor))
        | unsupported -> failwithf "unsupported xnode: %A" unsupported
      yield! lastLine ]

