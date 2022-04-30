namespace Conceal

open Elmish

module Conceal =
  type AppState =
    | FileSelector
    | Presentation
    | Browser

  type State =
    { CurrentAppState: AppState
      FileSelector: FileSelector.State
      Presentation: Presentation.State
      Browser: Browser.State
      AppInfo: AppInfo }

  type Msg =
    | FileSelectorMsg of FileSelector.Msg
    | PresentationMsg of Presentation.Msg
    | BrowserMsg of Browser.Msg

  let init appInfo =
    let path = appInfo.InputFilePath |> Option.defaultValue ""
    let state, fs, p =
      match SlidesLoader.load appInfo.Style path with
      | Some pages ->
          Presentation, FileSelector.init (), Presentation.init pages
      | None ->
          FileSelector, FileSelector.init (), Presentation.init [||]
    { CurrentAppState = state
      FileSelector = fs
      Presentation = p
      Browser = Browser.init ""
      AppInfo = appInfo }

  let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | FileSelectorMsg msg ->
        let fsState, cmd = FileSelector.update msg state.FileSelector
        let newState =
          if fsState.StartPresentation then
            match SlidesLoader.load state.AppInfo.Style fsState.InputFilePath with
            | Some pages ->
                { state with CurrentAppState = Presentation; Presentation = Presentation.init pages }
            | None ->
                 // 読み込み失敗(何かメッセージでも出す？)
                { state with FileSelector = { fsState with StartPresentation = false } }
          else
            { state with FileSelector = fsState }
        (newState, (cmd |> Cmd.map FileSelectorMsg))
    | PresentationMsg msg ->
        let pState, cmd = Presentation.update msg state.Presentation
        let newState =
          match pState.Browser with
          | Some url ->
              { state with CurrentAppState = Browser; Browser = Browser.init url }
          | None ->
              { state with Presentation = pState }
        newState, (cmd |> Cmd.map PresentationMsg)
    | BrowserMsg msg ->
        let bState, cmd = Browser.update msg state.Browser
        let newState =
          if bState.Close then
            { state with CurrentAppState = Presentation }
          else
            { state with Browser = bState }
        newState, (cmd |> Cmd.map BrowserMsg)

  let view (info: AppInfo) (state: State) (dispatch: Msg -> unit) =
    match state.CurrentAppState with
    | FileSelector ->
        FileSelector.view state.FileSelector (FileSelectorMsg >> dispatch)
    | Presentation ->
        Presentation.view info state.Presentation (PresentationMsg >> dispatch)
    | Browser ->
        Browser.view info state.Browser (BrowserMsg >> dispatch)
