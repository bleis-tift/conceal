namespace Conceal

open System
open FParsec

module ColorizerLaTeX =
  type Elem =
    | Command of Command
    | Text of string
  and Command =
    { Command: string
      Args: Arg list }
  and Arg =
    | Option of string
    | Option2 of string
    | Block of Elem list

  let private trace point x =
    // printfn "%s: %A" point x
    x

  let private ws = spaces

  let private platex, platexRef = createParserForwardedToRef ()

  let private pcmdName =
    pchar '\\' >>. (noneOf " [{<,>}]" |> many1Chars) .>> ws
    |>> trace "pcmdName"

  let private popt =
    noneOf "]" |> manyChars |> between (pchar '[') (pchar ']' >>. ws)
    |>> Option
    |>> trace "popt"

  let private popt2 =
    noneOf ">" |> manyChars |> between (pchar '<') (pchar '>' >>. ws)
    |>> Option2
    |>> trace "popt2"

  let private pblock =
    platex |> between (pchar '{') (pchar '}' >>. ws)
    |>> Block
    |>> trace "pblock"

  let private parg =
    choice [ popt; popt2; pblock ]
    |>> trace "parg"

  let private pcmd =
    pcmdName .>>. many parg
    |>> (fun (cmd, args) -> Command { Command = cmd; Args = args })
    |>> trace "pcmd"

  let private ptext =
    parse {
      let! txt = noneOf "\\{}[]<>" |> many1Chars
      if txt.Trim() = "" then
        return! fail "ptext(empty)"
      else
        return Text txt
    }
    |>> trace "ptext"

  let private pelem =
    pcmd <|> ptext
    |>> trace "pelem"

  let private parseCode (code: string) =
    platexRef :=
      many pelem .>> ws
      |>> trace "platex"
    match run (ws >>. platex .>> eof) code with
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwithf "parse error: %s" msg

  let private grouping (elems: Elem list) : Elem list list =
    let res = ResizeArray()
    let buf = ResizeArray()
    for elem in elems do
      match elem with
      | Command { Command = "item"; Args = _ } ->
          buf.Add(elem)
      | Command _ ->
          if buf.Count <> 0 then
            res.Add(buf |> Seq.toList)
            buf.Clear()
          res.Add([elem])
      | Text txt ->
          if buf.Count <> 0 then
            buf.Add(Text (" " + txt))
            res.Add(buf |> Seq.toList)
            buf.Clear()
          else
            res.Add([elem])
    if buf.Count <> 0 then
      res.Add(buf |> Seq.toList)
    res |> Seq.toList

  let rec private colorizeElem (style: Style) (elem: Elem) : Conceal.Text list =
    let cs = style.CodeStyles
    match elem with
    | Command { Command = cmd; Args = args } ->
        match cmd with
        | "documentclass"
        | "usepackage" ->
            [Text.Create([|
               TextElement.CreateCode("\\" + cmd, cs.KeywordColor2)
               for arg in args do
                 match arg with
                 | Option opt ->
                     TextElement.CreateCode("[", cs.DefaultColor)
                     TextElement.CreateCode(opt, cs.IdentifierColor1)
                     TextElement.CreateCode("]", cs.DefaultColor)
                 | Option2 opt -> 
                     TextElement.CreateCode("<" + opt + ">", cs.DefaultColor)
                 | Block block ->
                     TextElement.CreateCode("{", cs.DefaultColor)
                     for (Text t) in block do
                       TextElement.CreateCode(t, cs.UpperIdentifierColor)
                     TextElement.CreateCode("}", cs.DefaultColor)
             |])]
        | "renewcommand" ->
            [Text.Create([|
               yield TextElement.CreateCode("\\renewcommand", cs.KeywordColor1)
               for (Block elems) in args do
                 yield TextElement.CreateCode("{", cs.DefaultColor)
                 let texts = elems |> List.collect (colorizeElem style)
                 yield! texts |> List.collect (fun t -> t.Elements)
                 yield TextElement.CreateCode("}", cs.DefaultColor)
             |])]
        | "frame" ->
            [
              yield Text.Create([|
                TextElement.CreateCode("\\frame", cs.KeywordColor3)
                for arg in args do
                  match arg with
                  | Option opt -> TextElement.CreateCode("[" + opt + "]", cs.DefaultColor)
                  | Option2 opt -> TextElement.CreateCode("<" + opt + ">", cs.DefaultColor)
                  | Block _ -> ()
                TextElement.CreateCode(" {", cs.DefaultColor)
              |])
              for arg in args do
                match arg with
                | Option _
                | Option2 _ -> ()
                | Block block ->
                    for line in grouping block do
                      let texts = line |> List.collect (colorizeElem style)
                      yield Text.Create([|
                        yield TextElement.CreateCode("  ", cs.DefaultColor)
                        for t in texts do
                          yield! t.Elements
                      |])
              yield Text.Create([| TextElement.CreateCode("}", cs.DefaultColor) |])
            ]
        | _ ->
            [Text.Create([|
               TextElement.CreateCode("\\" + cmd, cs.KeywordColor3)
               for arg in args do
                 match arg with
                 | Option opt -> TextElement.CreateCode("[" + opt + "]", cs.DefaultColor)
                 | Option2 opt -> TextElement.CreateCode("<" + opt + ">", cs.DefaultColor)
                 | Block block ->
                     TextElement.CreateCode("{", cs.DefaultColor)
                     for elem in block do
                       match elem with
                       | Command cmd ->
                           TextElement.CreateCode("\\" + cmd.Command, cs.KeywordColor3)
                       | Text txt ->
                           TextElement.CreateCode(txt.TrimEnd(), cs.DefaultColor)
                     TextElement.CreateCode("}", cs.DefaultColor)
             |])]
    | Text text ->
        [Text.Create([|TextElement.CreateCode(text.TrimEnd(), cs.DefaultColor)|])]

  let private colorizeLaTeX (style: Style) (tree: Elem list) =
    tree |> List.collect (colorizeElem style)

  let colorize (style: Style) (code: string) =
    let parsed = parseCode code
    colorizeLaTeX style parsed

