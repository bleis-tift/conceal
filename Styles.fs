namespace Conceal

module Styles =
  let dark fontName =
    let codeStyle =
      { Background = { A = 255uy; R = 67uy; G = 69uy; B = 61uy }
        FontName = fontName
        CommentColor = { A = 255uy; R = 106uy; G = 153uy; B = 85uy }
        DefaultColor = { A = 255uy; R = 212uy; G = 212uy; B = 212uy }
        IdentifierColor1 = { A = 255uy; R = 156uy; G = 220uy; B = 254uy }
        IdentifierColor2 = { A = 255uy; R = 78uy; G = 201uy; B = 176uy }
        InactiveCodeColor = { A = 255uy; R = 166uy; G = 166uy; B = 166uy }
        KeywordColor1 = { A = 255uy; R = 86uy; G = 156uy; B = 214uy }
        KeywordColor2 = { A = 255uy; R = 197uy; G = 134uy; B = 192uy }
        LineCommentColor = { A = 255uy; R = 106uy; G = 153uy; B = 85uy }
        NumberColor = { A = 255uy; R = 181uy; G = 206uy; B = 168uy }
        OperatorColor = { A = 255uy; R = 86uy; G = 156uy; B = 214uy }
        PreprocessorKeywordColor = { A = 255uy; R = 0uy; G = 0uy; B = 0uy } // TODO
        PunctuationColor = { A = 255uy; R = 86uy; G = 156uy; B = 214uy }    // TODO
        StringColor = { A = 255uy; R = 206uy; G = 145uy; B = 120uy }
        TextColor = { A = 255uy; R = 206uy; G = 145uy; B = 120uy }
        UpperIdentifierColor = { A = 255uy; R = 78uy; G = 201uy; B = 176uy }
        ResultBackground = { A = 220uy; R = 0uy; G = 0uy; B = 0uy }
        ErrorColor = { A = 255uy; R = 255uy; G = 0uy; B = 0uy } }
    { TextColor = { A = 255uy; R = 222uy; G = 222uy; B = 222uy }
      LinkColor = { A = 255uy; R = 19uy; G = 218uy; B = 236uy }
      CodeStyles = codeStyle
      TitleSizeRate = 1.0 / 7.0
      HeaderSizeRate = 1.0 / 9.0
      TextSizeRate = 1.0 / 11.0 }
