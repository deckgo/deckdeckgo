# deckgo-social

## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/youtube@latest/dist/deckdeckgo-youtube/deckdeckgo-youtube.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/youtube
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/youtube';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/youtube/dist/loader';
deckDeckGoElement();
```

## Usage

The "YouTube" slide's Web Component could be integrated using the tag `<deckgo-youtube/>`.

```
<deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
</deckgo-youtube>
```

<!-- Auto Generated Below -->


## Properties

| Property          | Attribute          | Description                                                                                                                              | Type      | Default     |
| ----------------- | ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------- |
| `allowFullscreen` | `allow-fullscreen` | Allow option to toggle video in full screen                                                                                              | `boolean` | `true`      |
| `frameTitle`      | `frame-title`      | A title for the frame, could be use for accessibility reason                                                                             | `string`  | `undefined` |
| `height`          | `height`           | The height of the video player                                                                                                           | `number`  | `undefined` |
| `instant`         | `instant`          | In case you would like to load the video as soon as the component is loaded                                                              | `boolean` | `false`     |
| `src`             | `src`              | The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube | `string`  | `undefined` |
| `width`           | `width`            | The width of the video player                                                                                                            | `number`  | `undefined` |


## Methods

### `lazyLoadContent() => Promise<void>`

Lazy load the video

#### Returns

Type: `Promise<void>`



### `pause() => Promise<void>`

Pause the video

#### Returns

Type: `Promise<void>`



### `play() => Promise<void>`

Play the video

#### Returns

Type: `Promise<void>`



### `updateIFrame(width: number, height: number) => Promise<void>`

Update the iFrame, the video, size

#### Returns

Type: `Promise<void>`




## CSS Custom Properties

| Name                                  | Description                                                   |
| ------------------------------------- | ------------------------------------------------------------- |
| `--deckgo-youtube-opacity-loaded`     | The opacity of the video once loaded @default 1               |
| `--deckgo-youtube-opacity-not-loaded` | The opacity of the video when not loaded @default 0           |
| `--deckgo-youtube-transition`         | The transition of the container @default opacity 0.15s linear |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
