# DeckDeckGo - Extra Components

[DeckDeckGo] provide a couple of extra components which could be use in any slides of your presentation.

## Table of contents

- [Chart](#chart)
- [Gif](#gif)
- [Highlight Code](#highlight-code)
- [QR Code](#qr-code)
- [Social](#social)
- [Youtube](#youtube)

## Chart

The "Chart" component is an extra component which let you draw charts easily. This Web Component could be use in [DeckDeckGo] or in any other web projects. See its [documentation](https://github.com/fluster/deckdeckgo-charts) for the details.

## Gif

The "Gif" component allows you to easily add a Gif, like those provided by [Giphy](https://giphy.com), in almost any slide of your presentation.

### Usage

The "Gif" slide's Web Component could be integrated using the tag `<deckgo-gif/>`.

```
<deckgo-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

#### Slots

The slots `header` and `footer` are both optional. `header` and `footer` would be displayed over the gif.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src | string |  | The source url, the src, of the Gif. Could be an embeddable external url or a local one. |
| alt | string |  | And alt information could be provided for accessibility reason. |
| fullscreen | number | false | If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen. |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --width | | The width of the gif |
| --height | | The height of the gif |
| --background | | The background of the `header` and `footer` over the gif |
| --color | | The color of the `header` and `footer` over the gif|
| --padding | | The padding of the `header` and `footer` over the gif |
| --zIndex | 2 | The z-index of the slide |

### Note

Of course, as other images added to a presentation build with [DeckDeckGo], gifs are lazy loaded too. 

## Highlight Code

The "Highlight Code" component is an extra component which let you highlight code easily. This Web Component could be use in [DeckDeckGo] or in any other web projects. See its [documentation](https://github.com/fluster/deckdeckgo-highlight-code) for the details.

## QR Code

The "QR Code" component is an extra component which let you add QR code in your slides, useful for example to display links and url and if you wish your audience to easily access them. This Web Component could be use in [DeckDeckGo] or in any other web projects. See its [documentation](https://github.com/fluster/deckdeckgo-qr-code) for the details.

## Social

The "Social" component allows you to easily add a social link to your presentation.

### Usage

The "Social" slide's Web Component could be integrated using the tag `<deckgo-social/>`.

```
<deckgo-social twitter="daviddalbusco">
  <img data-src="/assets/twitter.svg" slot="icon"/>
  twitter
</deckgo-social>
```

#### Slots

The slot `icon` and the text are both optional. Of course, if you provide nothing, nothing will be rendered.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| twitter | string |  | Your Twitter username. It will be concatenated automatically with `https://twitter.com/` |
| linkedin | string |  | Your Linkedin username. It will be concatenated automatically with `https://www.linkedin.com/in/` |
| medium | string |  | Your Medium username. It will be concatenated automatically with `https://medium.com/@` |
| github | string |  | Your Github username. It will be concatenated automatically with `https://github.com/` |
| fullUrl | string |  | In case you would like to provide the URI of your choice |

### Examples

Without any icons:

```
<deckgo-social twitter="daviddalbusco">Twitter</deckgo-social>
<deckgo-social linkedin="david-dal-busco/">Linkedin</deckgo-social>
<deckgo-social medium="david.dalbusco">Medium</deckgo-social>
<deckgo-social full-url="https://stackoverflow.com/users/5404186/peter-parker">Stackoverflow</deckgo-social>
```

With for example `ion-icon`:

```
<deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon> Twitter</deckgo-social>
<deckgo-social github="fluster/deckdeckgo"><ion-icon slot="icon" name="logo-github"></ion-icon> DeckDeckGo on Github</deckgo-social>
```

## Youtube

The "Youtube" component allows you to easily add a [Youtube](https://youtube.com) video in almost any slide of your presentation.

### Usage

The "Youtube" slide's Web Component could be integrated using the tag `<deckgo-youtube/>`.

```
<deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
</deckgo-youtube>
```

#### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src | string |  | The source url, the Youtube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by Youtube. |
| width | number |  | The width of the video player. |
| height | number |  | The height of the video player. |
| frame-title | string |  | A title for the frame, could be use for accessibility reason. | 

[DeckDeckGo]: https://deckdeckgo.com