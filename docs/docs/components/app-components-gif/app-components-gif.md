# Gif

The "Gif" component allows you to easily add a Gif, like those provided by [Giphy](https://giphy.com), in almost any slide of your presentation.

## Table of contents

- [Showcase](#app-components-gif-showcase)
- [Usage](#app-components-gif-usage)
  - [Slots](#app-components-gif-slots)
- [Attributes](#app-components-gif-attributes)
- [Theming](#app-components-gif-theming)
- [Note](#app-components-gif-note)

## Showcase

<div>
  <deckgo-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" style={{'--width': '100%'}}>
    <h1 slot="header">Hey</h1>
    <h2 slot="footer">It's a cool gif</h2>
  </deckgo-gif>
</div>

## Usage

The "Gif" slide's Web Component could be integrated using the tag `<deckgo-gif/>`.

```
<deckgo-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

### Slots

The slots `header` and `footer` are both optional. `header` and `footer` would be displayed over the gif.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| src | string |  | The source url, the src, of the Gif. Could be an embeddable external url or a local one. |
| alt | string |  | And alt information could be provided for accessibility reason. |
| fullscreen | number | false | If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen. |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --width | | The width of the gif |
| --height | | The height of the gif |
| --background | | The background of the `header` and `footer` over the gif |
| --color | | The color of the `header` and `footer` over the gif|
| --padding | | The padding of the `header` and `footer` over the gif |
| --zIndex | 2 | The z-index of the slide |

## Note

Of course, as other images added to a presentation build with [DeckDeckGo], gifs are lazy loaded too. 

[DeckDeckGo]: https://deckdeckgo.com