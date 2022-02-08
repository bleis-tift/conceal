namespace Conceal

open Avalonia.Controls
open Avalonia.FuncUI.DSL

module Conceal =
  type State = unit

  type Message = unit

  let update (msg: Message) (state: State) : State =
    ()

  let view (state: State) dispatch =
    TextBlock.create [
      TextBlock.text "sample"
    ]
