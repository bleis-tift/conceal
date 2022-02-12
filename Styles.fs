namespace Conceal

module Styles =
  let dark fontName =
    let codeStyle =
      { Background = { A = 255uy; R = 67uy; G = 69uy; B = 61uy }
        FontName = fontName }
    { TextColor = { A = 255uy; R = 222uy; G = 222uy; B = 222uy }
      LinkColor = { A = 255uy; R = 19uy; G = 218uy; B = 236uy }
      CodeStyles = codeStyle
      TitleSizeRate = 1.0 / 7.0
      HeaderSizeRate = 1.0 / 9.0
      TextSizeRate = 1.0 / 11.0 }
