# deckgo-slide-youtube

The "YouTube" slide let you add easily a YouTube video to your presentation.

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-youtube@latest/dist/deckdeckgo-slide-youtube/deckdeckgo-slide-youtube.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-youtube) run the following command:

```bash
npm install @deckdeckgo/slide-youtube
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-youtube';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-youtube/dist/loader';
deckDeckGoSlideElement();
```

### YouTube component

This template relies on the `@deckdeckgo/youtube` component without any explicit dependency. Therefore it should also be installed.

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                                                                                                | Type      | Default     |
| ------------------ | ------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------- |
| `allowFullscreen`  | `allow-fullscreen`  | Allow fullscreen option                                                                                                                                                    | `boolean` | `true`      |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true                                                                        | `boolean` | `false`     |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true                                                                    | `boolean` | `false`     |
| `height`           | `height`            | Per default the video height will be calculated according the content size available. Per default the video height will be calculated according the content size available | `number`  | `undefined` |
| `src`              | `src`               | The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube                                   | `string`  | `undefined` |
| `width`            | `width`             | Per default the video width will be calculated according the content size available. Using this option you would be able to define your own width                          | `number`  | `undefined` |


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



### `pause() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `play() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `toggle() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot           | Description                        |
| -------------- | ---------------------------------- |
| `"actions"`    | Custom actions for this slide      |
| `"background"` | A custom background for this slide |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                       | Description                                                        |
| -------------------------- | ------------------------------------------------------------------ |
| `--background`             | background                                                         |
| `--color`                  | color                                                              |
| `--overflow`               | overflow of the slide @default hidden                              |
| `--slide-padding-bottom`   | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-padding-end`      | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-padding-start`    | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-padding-top`      | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-user-select`      | user select @default none                                          |
| `--slide-youtube-height`   | The height of the video's container @default calc(100% - 32px)     |
| `--slide-youtube-margin`   | The margin of the video's container @default 0 0 32px              |
| `--slide-youtube-overflow` | The overflow of the video's container @default auto                |
| `--zIndex`                 | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
