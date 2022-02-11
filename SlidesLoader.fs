namespace Conceal

module SlidesLoader =
  let load (style: Style) (path: string) =
    // TODO : impl
    Some [|
      Page.Create(
        TitlePage,
        [PageContent.CreateText(Text.Create(TextElement.Create("Title", style.TextColor)))],
        [])
      Page.Create(
        ContentPage,
        [PageContent.CreateText(Text.Create(TextElement.Create("Header", style.TextColor)))],
        [PageContent.CreateText(Text.Create(TextElement.Create("sample body", style.TextColor)))])
      Page.Create(
        ContentPage,
        [PageContent.CreateText(Text.Create(TextElement.Create("Header", style.TextColor)))],
        [PageContent.CreateList([
           [PageContent.CreateText(Text.Create(TextElement.Create("item1", style.TextColor)))]
           [PageContent.CreateText(Text.Create(TextElement.Create("item2", style.TextColor)))
            PageContent.CreateList([
              [PageContent.CreateText(Text.Create(TextElement.Create("sub-item1", style.TextColor)))]
              [PageContent.CreateText(Text.Create(TextElement.Create("sub-item2", style.TextColor)))]
            ])]
           [PageContent.CreateText(Text.Create(TextElement.Create("item3", style.TextColor)))]
         ])])
    |]
