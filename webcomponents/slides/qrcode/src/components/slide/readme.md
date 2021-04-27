# deckgo-slide-qrcode

The "QR code" slide is an handy slide in case you would like to display a QR code. It could for example be use as the very last slide of your presentation to display an easy link pointing to your deck, you previously published online. It would allow your audience to get easily your slides without any delay on their phone.

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

## Code components

The slide "QR Code" relies on the code component `<deckgo-qrcode/>`.

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                                                     | Type      | Default     |
| ------------------ | ------------------- | ------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------- |
| `content`          | `content`           | The content, a text or an url, of the QR code to generate                                                                       | `string`  | `undefined` |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true                             | `boolean` | `false`     |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true                         | `boolean` | `false`     |
| `imgAlt`           | `img-alt`           | In case you would display a logo over the QR code, you could provide an accessibility attribute using this option               | `string`  | `undefined` |
| `imgSrc`           | `img-src`           | In case you would like to display a logo over the QR code, provide the source of the image. Note: this image is lazy loaded too | `string`  | `undefined` |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(_enter: boolean, _reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot           | Description                        |
| -------------- | ---------------------------------- |
| `"actions"`    | Custom actions for this slide      |
| `"background"` | A custom background for this slide |
| `"content"`    | An optional content                |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                           | Description                                                        |
| ------------------------------ | ------------------------------------------------------------------ |
| `--background`                 | background                                                         |
| `--color`                      | color                                                              |
| `--overflow`                   | overflow of the slide @default hidden                              |
| `--slide-padding-bottom`       | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-padding-end`          | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-padding-start`        | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-padding-top`          | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-qrcode-align`         | QR code vertical alignment @default center                         |
| `--slide-qrcode-background`    | QR code column's background                                        |
| `--slide-qrcode-text-align`    | QR code horizontal alignment @default center                       |
| `--slide-qrcode-title-display` | If you wish to hide the slot="title" @default bloack               |
| `--slide-user-select`          | user select @default none                                          |
| `--zIndex`                     | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
