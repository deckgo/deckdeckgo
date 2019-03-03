# Slide: QR Code

The "QR code" slide is an handy slide in case you would like to display a QR code. It could for example be use as the very last slide of your presentation to display an easy link pointing to your deck, you previously published online. It would allow your audience to get easily your slides without any delay on their phone.

## Table of contents

- [Layout](#app-slide-qrcode-layout)
- [Usage](#app-slide-qrcode-usage)
  - [Usage](#app-slide-qrcode-usage-1)
  - [Slots](#app-slide-qrcode-slots)
  - [Notes](#app-slide-qrcode-notes)
- [Code components](#app-slide-qrcode-code-components)
- [Installation](#app-slide-qrcode-installation)
- [Attributes](#app-slide-qrcode-attributes)
  - [Example without any slots](#app-slide-qrcode-example-without-any-slots)
- [Theming](#app-slide-qrcode-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

## Usage

The "QR code" slide's Web Component could be integrated using the tag `<deckgo-slide-qrcode/>`.

### Usage

```
<deckgo-deck>
  <deckgo-slide-qrcode content="https://deckdeckgo.com">
    <h1 slot="title">My QR code</h1>
    <p slot="content">An optional additional content</p>
  </deckgo-slide-code>
</deckgo-deck>  
```

### Slots

The slots `title` and `content` are optional.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Code components

The slide "QR Code" relies on the code component `<deckgo-qrcode/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Installation

The [DeckDeckGo] - QR Code component is provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] QR code to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/deckdeckgo-qrcode) as described in its [installation guide](https://github.com/deckgo/deckdeckgo-qrcode#getting-started).

## Attributes

The attribute `content` should be provided in order to render a QR code in this template. It offers the same attributes as the [DeckDeckGo] QR code Web Component, see its [documentation](https://github.com/deckgo/deckdeckgo-qrcode) for the details.

### Example without any slots

```
<deckgo-deck>
  <deckgo-slide-qrcode content="An encoded text">
  </deckgo-slide-code>
</deckgo-deck>  
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --slide-qrcode-align | center | QR code vertical alignment |
| --slide-qrcode-text-align | center | QR code horizontal alignment |
| --slide-qrcode-background | | QR code column's background |
| --slide-qrcode-title-display | inherit | If you wish to hide the slot="title" |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] - QR code Web Component, see its [documentation](https://github.com/deckgo/deckdeckgo-qrcode) for the details.

[DeckDeckGo]: https://deckdeckgo.com