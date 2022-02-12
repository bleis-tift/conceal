namespace Conceal

open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open WebViewControl

module WebView =
  type WebView with
    static member create (attrs: IAttr<WebView> list) : IView<WebView> =
      ViewBuilder.Create<WebView>(attrs)

    static member address (address: string) =
      AttrBuilder<WebView>.CreateProperty(
        WebView.AddressProperty,
        address,
        ValueNone
      )
