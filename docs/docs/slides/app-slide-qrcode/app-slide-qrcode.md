# Slide: QR Code

The "QR code" slide is an handy slide in case you would like to display a QR code. It could for example be use as the very last slide of your presentation to display an easy link pointing to your deck, you previously published online. It would allow your audience to get easily your slides without any delay on their phone.

## Table of contents

- [Layout](#app-slide-qrcode-layout)
- [Installation](#app-slide-qrcode-installation)
  - [From a CDN](#app-slide-qrcode-from-a-cdn)
  - [From NPM](#app-slide-qrcode-from-npm)
  - [Framework integration](#app-slide-qrcode-framework-integration)
- [Usage](#app-slide-qrcode-usage)
  - [Usage](#app-slide-qrcode-usage-1)
  - [Slots](#app-slide-qrcode-slots)
- [Code components](#app-slide-qrcode-code-components)
- [Installation](#app-slide-qrcode-installation)
- [Attributes](#app-slide-qrcode-attributes)
  - [Example without any slots](#app-slide-qrcode-example-without-any-slots)
- [Theming](#app-slide-qrcode-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-qrcode content="https://deckdeckgo.com">
        <h1 slot="title">slot="title"</h1>
        <p slot="content">slot="content"</p>
    </deckgo-slide-qrcode>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-qrcode@latest/dist/deckdeckgo-slide-qrcode/deckdeckgo-slide-qrcode.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-qrcode) run the following command:

```bash
npm install @deckdeckgo/slide-qrcode
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-qrcode';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-qrcode/dist/loader';
deckDeckGoSlideElement();
```

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

## Code components

The slide "QR Code" relies on the code component `<deckgo-qrcode/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Installation

The [DeckDeckGo] - QR Code component is provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] QR code to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) as described in its [installation guide](https://docs.deckdeckgo.com/components/qrcode#app-components-qrcode-getting-started).

## Attributes

The attribute `content` should be provided in order to render a QR code in this template. It offers the same attributes as the [DeckDeckGo] QR code Web Component, see its [documentation](https://docs.deckdeckgo.com/components/qrcode) for the details, and the following other attributes:

| Attribute         | Type    | Default | Description                                                                                                                      |
| ----------------- | ------- | ------- | -------------------------------------------------------------------------------------------------------------------------------- |
| img-src           | string  |         | In case you would like to display a logo over the QR code, provide the source of the image. Note: this image is lazy loaded too. |
| img-alt           | string  |         | In case you would display a logo over the QR code, you could provide an accessibility attribute using this option.               |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true`                  |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`                       |

### Example without any slots

```
<deckgo-deck>
  <deckgo-slide-qrcode content="An encoded text">
  </deckgo-slide-code>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                | Default | Note                                 |
| ---------------------------- | ------- | ------------------------------------ |
| --background                 |         |                                      |
| --color                      |         |                                      |
| --slide-padding-top          | 16px    | Padding top of the all slide         |
| --slide-padding-end          | 32px    | Padding right of the all slide       |
| --slide-padding-bottom       | 16px    | Padding bottom of the all slide      |
| --slide-padding-start        | 32px    | Padding left of the all slide        |
| --slide-qrcode-align         | center  | QR code vertical alignment           |
| --slide-qrcode-text-align    | center  | QR code horizontal alignment         |
| --slide-qrcode-background    |         | QR code column's background          |
| --slide-qrcode-title-display | block   | If you wish to hide the slot="title" |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] - QR code Web Component, see its [documentation](https://docs.deckdeckgo.com/components/qrcode) for the details.

[deckdeckgo]: https://deckdeckgo.com
