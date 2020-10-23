# QR Code

The "QR Code" component is an extra component which let you add QR code in your slides, useful for example to display links and url and if you wish your audience to easily access them.

To generate the QR code, the project [qrcode-generator](https://github.com/kazuhikoarase/qrcode-generator) from [Kazuhiko Arase](https://github.com/kazuhikoarase) is used.

## Table of contents

- [Showcase](#app-components-qrcode-showcase)
- [Installation](#app-components-qrcode-installation) - [Using DeckDeckGo QR Code from a CDN](#app-components-qrcode-using-deckdeckgo-qr-code-from-a-cdn) - [Install DeckDeckGo QR Code from NPM](#app-components-qrcode-install-deckdeckgo-qr-code-from-npm) - [Framework integration](#app-components-qrcode-framework-integration)
- [Usage](#app-components-qrcode-usage) - [Slot](#app-components-qrcode-slot) - [Properties](#app-components-qrcode-properties) - [Styling](#app-components-qrcode-styling) - [Styling type img](#app-components-qrcode-styling-type-img)
  - [Methods](#app-components-qrcode-methods)
  - [Examples](#app-components-qrcode-examples)
- [QR Code with logo](#app-components-qrcode-qr-code-with-logo)

## Showcase

This Web Component let you generate QR code like the following as `svg` (default) or `img`:

<div>
  <deckgo-qrcode content="https://deckdeckgo.com" style={{'--deckgo-qrcode-size': '300px', '--deckgo-qrcode-color-fill': 'var(--ion-color-primary)'}}>
  </deckgo-qrcode>
</div>

Optionally you could also display a logo over your QR code:

<div>
  <deckgo-qrcode content="https://deckdeckgo.com" style={{'--deckgo-qrcode-size': '300px', '--deckgo-qrcode-color-fill': 'var(--ion-color-primary)'}}>
    <img slot="logo" src="/assets/img/deckdeckgo-logo.svg"/>
  </deckgo-qrcode>
</div>

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using DeckDeckGo QR Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/qrcode@latest/dist/deckdeckgo-qrcode/deckdeckgo-qrcode.esm.js"></script>
```

### Install DeckDeckGo QR Code from NPM

Install [DeckDeckGo] - QR Code in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

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

### Slot

To display a logo over your QR code, this Web Component provide a `slot` called `logo`.

### Properties

The `<deckgo-qrcode/>` expose the following properties:

| Property            | Attribute                 | Description                                                                                                                                              | Type     | Default | Only applies for type `<img/>` |
| ------------------- | ------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- | -------- | ------- | ------------------------------ |
| `content`           | `content`                 | The content, a text or an url, of the QR code to generate                                                                                                | `string` |         |                                |
| `type`              | `type`                    | The type of QR code to generate, `<svg/>` or `<img/>`                                                                                                    | `string` | `svg`   |                                |
| `qrCellSize`        | `qr-cell-size`            | The size of the cell, useful to generate a bigger QR code, specially in case of `<img/>`. Use it wisely, I suggest a value between 0 and 20 for example. | `number` |         |                                |
| `qrMargin`          | `qr-margin`               | The size of the code margin, in case you would like more spacing                                                                                         | `number` |         |                                |
| `qrAlt`             | `qr-img-alt`              | An alternate text for the image of the QR code                                                                                                           | `string` |         | X                              |
| `qrBackgroundColor` | `qr-img-background-color` | The background color of the QR code. The value should be provided in a RGB-hex format. For example: `FF0000`.                                            | `string` |         | X                              |
| `qrFillColor`       | `qr-img-fill-color`       | The color use to fill the QR code. The value should be provided in a RGB-hex format. For example: `FF0000`.                                              | `string` |         | X                              |

### Styling

The `<deckgo-qrcode/>` could be styled using the following CSS4 variables which would only applies on the type `<svg/>`:

| CSS4 variable                     | Default      | Note                                                                                                                                                      | Only applies for type `<svg/>` |
| --------------------------------- | ------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| --deckgo-qrcode-container-display | inline-block | The display property of the shadow host and the code container                                                                                            |                                |
| --deckgo-qrcode-size              |              | The size, width and height, of the QR code                                                                                                                | X                              |
| --deckgo-qrcode-logo-ratio-size   | 3            | If you are injecting a logo, its size, width and height, will be calculated with `--deckgo-qrcode-size` (or `100%` if not provided) divided by this value | X                              |
| --deckgo-qrcode-border-stroke     |              | The border color of the QR code                                                                                                                           | X                              |
| --deckgo-qrcode-background-fill   | transparent  | The QR code's background                                                                                                                                  | X                              |
| --deckgo-qrcode-color-fill        |              | The QR code's color (the color of the QR code's squares it contains)                                                                                      | X                              |

#### Styling type img

In oder to style QR code if its type is set to `<img/>`, you will need to use properties instead of CSS4 variables.

### Methods

The `<deckgo-qrcode/>` component exposes the following method in case you would like to refresh your QR code, for example on resize of the window on in case you would set its content asynchronously:

```
generate() => Promise<void>
```

### Examples

You could find all the examples in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/qrcode/src/index.html) of the project.

```
<deckgo-qrcode content="https://deckdeckgo.com" style="--deckgo-qrcode-size: 300px;">
</deckgo-qrcode>
```

Example with a logo:

```
<deckgo-qrcode content="https://myurl.com">
  <img slot="logo" src="my-logo.svg"/>
</deckgo-qrcode>
```

## QR Code with logo

It's possible to display a logo over your QR Code as the code generated with this Web Component have a correction level set to high meaning, if I understand correctly, that your content is encoded and displayed multiple times inside the QR code. Therefore, even if the logo cover a part of it, it will be still possible for a reader to read the content from "somewhere else" in the code.

However, test it carefully and play with the colours, cell-size and size of your code to ensure its readability.

[deckdeckgo]: https://deckdeckgo.com
