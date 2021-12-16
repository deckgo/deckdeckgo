# deckgo-qrcode

The "QR Code" component is an extra component which let you add QR code in your slides, useful for example to display links and url and if you wish your audience to easily access them.

To generate the QR code, the project [qrcode-generator](https://github.com/kazuhikoarase/qrcode-generator) from [Kazuhiko Arase](https://github.com/kazuhikoarase) is used.

## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/elements@latest/dist/elements/elements.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/elements) using the following command:

```bash
npm install @deckdeckgo/elements
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/elements';
```

#### Loader

```
import { defineCustomElements } from '@deckdeckgo/elements/dist/loader';
defineCustomElements();
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


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
