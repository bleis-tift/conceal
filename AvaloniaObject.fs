namespace Conceal

open Avalonia

module AvaloniaObject =
  let colorFrom (color: Color) =
    Media.Color.FromArgb(color.A, color.R, color.G, color.B)

  let brushFrom (color: Color) =
    Media.SolidColorBrush(colorFrom color)

  let fontFamilyFrom (fontName: string) =
    Media.FontFamily(fontName)
