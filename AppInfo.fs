namespace Conceal

open System

type AppInfo =
  { Width: int
    Height: int
    Style: Style
    InputFilePath: string option }

module AppInfo =
  let (|Integer|_|) (str: string) =
    match Int32.TryParse(str) with
    | true, result -> Some result
    | false, _ -> None

  let (|OptionName|_|) (prefix: string) (str: string) =
    if str.StartsWith(prefix) then
      Some (str.Substring(prefix.Length))
    else
      None

  let parse codeFontName args =
    let rec parse' acc = function
    | (Integer w)::(Integer h)::rest -> parse' { acc with Width = w; Height = h } rest
    | (OptionName "style=" "dark")::rest -> parse' { acc with Style = Styles.dark codeFontName } rest
    | path::rest -> parse' { acc with InputFilePath = Some path } rest
    | [] -> acc

    parse' { Width = 1600; Height = 900; Style = Styles.dark codeFontName; InputFilePath = None } (args |> Array.toList)
