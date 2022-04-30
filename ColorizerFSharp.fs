namespace Conceal

open System
open FSharp.Compiler.Tokenization

module ColorizerFSharp =
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

  let colorize (style: Style) (code: string) =
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

