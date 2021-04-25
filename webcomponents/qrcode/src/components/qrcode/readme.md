# deckgo-qrcode

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using DeckDeckGo QR Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/qrcode@latest/dist/deckdeckgo-qrcode/deckdeckgo-qrcode.esm.js"></script>
```

### Install DeckDeckGo QR Code from NPM

Install [DeckDeckGo](https://deckdeckgo.com) - QR Code in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/qrcode
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/qrcode';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/qrcode/dist/loader';
deckDeckGoElement();
```

## Usage

The `<deckgo-qrcode/>` Web Component will generate per default a `<svg/>` QR code with a correction level set to high.

Optionally, it's also possible to generate the QR code as an `<img/>` and/or to display a logo over it.

## QR Code with logo

It's possible to display a logo over your QR Code as the code generated with this Web Component have a correction level set to high meaning, if I understand correctly, that your content is encoded and displayed multiple times inside the QR code. Therefore, even if the logo cover a part of it, it will be still possible for a reader to read the content from "somewhere else" in the code.

However, test it carefully and play with the colours, cell-size and size of your code to ensure its readability.

<!-- Auto Generated Below -->


## Properties

| Property            | Attribute             | Description                                                                                                                                           | Type     | Default                    |
| ------------------- | --------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- | -------- | -------------------------- |
| `content`           | `content`             | The content, a text or an url, of the QR code to generate                                                                                             | `string` | `undefined`                |
| `qrAlt`             | `qr-alt`              | An alternate text for the image of the QR code                                                                                                        | `string` | `undefined`                |
| `qrBackgroundColor` | `qr-background-color` | The background color of the QR code. The value should be provided in a RGB-hex format. For example: FF0000                                            | `string` | `undefined`                |
| `qrCellSize`        | `qr-cell-size`        | The size of the cell, useful to generate a bigger QR code, specially in case of <img/>. Use it wisely, I suggest a value between 0 and 20 for example | `number` | `undefined`                |
| `qrFillColor`       | `qr-fill-color`       | The color use to fill the QR code. The value should be provided in a RGB-hex format. For example: FF0000                                              | `string` | `undefined`                |
| `qrMargin`          | `qr-margin`           | The size of the code margin, in case you would like more spacing                                                                                      | `number` | `undefined`                |
| `type`              | `type`                | The type of QR code to generate, <svg/> or <img/>                                                                                                     | `string` | `DeckdeckgoQRCodeType.SVG` |


## Methods

### `generate() => Promise<void>`

The <deckgo-qrcode/> component exposes the following method in case you would like to refresh your QR code, for example on resize of the window on in case you would set its content asynchronously.

#### Returns

Type: `Promise<void>`




## Slots

| Slot     | Description                                                |
| -------- | ---------------------------------------------------------- |
| `"logo"` | An optional logo or image to be displayed over the QR code |


## CSS Custom Properties

| Name                                | Description                                                                                                                                                      |
| ----------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `--deckgo-qrcode-background-fill`   | The QR code's background @default transparent                                                                                                                    |
| `--deckgo-qrcode-border-stroke`     | The border color of the QR code                                                                                                                                  |
| `--deckgo-qrcode-color-fill`        | The QR code's color (the color of the QR code's squares it contains)                                                                                             |
| `--deckgo-qrcode-container-display` | The display property of the shadow host and the code container @default inline-block                                                                             |
| `--deckgo-qrcode-logo-ratio-size`   | If you are injecting a logo, its size, width and height, will be calculated with --deckgo-qrcode-size (or 100% if not provided) divided by this value @default 3 |
| `--deckgo-qrcode-size`              | The size, width and height, of the QR code                                                                                                                       |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
