namespace Conceal

open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open Avalonia.Controls.Skia
open global.Svg.Skia
open SkiaSharp

module SKPictureControl =
  type SKPictureControl with
    static member create (attrs: IAttr<SKPictureControl> list) : IView<SKPictureControl> =
      ViewBuilder.Create<SKPictureControl>(attrs)

    static member height (height: float) =
      AttrBuilder<SKPictureControl>.CreateProperty(
        SKPictureControl.HeightProperty,
        height,
        ValueNone
      )

    static member stretch (stretch: Avalonia.Media.Stretch) =
      AttrBuilder<SKPictureControl>.CreateProperty(
        SKPictureControl.StretchProperty,
        stretch,
        ValueNone
      )

    static member picture (pict: SKPicture) =
      AttrBuilder<SKPictureControl>.CreateProperty(
        SKPictureControl.PictureProperty,
        pict,
        ValueNone
      )

