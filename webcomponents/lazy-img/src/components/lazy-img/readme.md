# deckgo-lazy-img

The "Lazy Image" component is a dead simple component to lazy load images.

It leverages the native lazy-loading and the IntersectionObserver API (default behavior) to lazy load images. It also allows you to trigger "manually" their loading.

An `<img/>` tag is per default use to display the image but optionally it could parse `SVG` too.

## Installation

This component can be added to your web application with following methods.

> If you are using our developer kit to create a presention, this component is already included

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/lazy-img@latest/dist/deckdeckgo-lazy-img/deckdeckgo-lazy-img.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/lazy-img
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/lazy-img';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/lazy-img/dist/loader';
deckDeckGoElement();
```

## Usage

The "Lazy Image" Web Component could be integrated using the tag `<deckgo-lazy-img/>`.

```
<deckgo-lazy-img img-src="/assets/twitter.svg">
</deckgo-lazy-img>
```

<!-- Auto Generated Below -->


## Properties

| Property             | Attribute              | Description                                                                                                                                                                                                                                                                  | Type                 | Default     |
| -------------------- | ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------- | ----------- |
| `ariaLabel`          | `aria-label`           | If you are using the above SVG option, provide the accessibility information using this attribute                                                                                                                                                                            | `string`             | `undefined` |
| `customLoader`       | `custom-loader`        | In case you would like to take care by yourself to apply the load of the image. If turn to true then the component will emit an event customLoad when the image intersect the viewport instead of displaying it (doesn't apply for svg but only for img-src and img-src-set) | `boolean`            | `false`     |
| `imgAlt`             | `img-alt`              | The image alternate text                                                                                                                                                                                                                                                     | `string`             | `undefined` |
| `imgErrorSrc`        | `img-error-src`        | An optional image which could be displayed in case the main image would not be resolved                                                                                                                                                                                      | `string`             | `undefined` |
| `imgHeight`          | `img-height`           | The image height                                                                                                                                                                                                                                                             | `number`             | `undefined` |
| `imgSizes`           | `img-sizes`            | The set of media conditions to indicates what image size would be best to choose                                                                                                                                                                                             | `string`             | `undefined` |
| `imgSrc`             | `img-src`              | The image source (= src) to lazy load                                                                                                                                                                                                                                        | `string`             | `undefined` |
| `imgSrcSet`          | `img-src-set`          | The attribute "srcset" (= multiple URI) to lazy load in case you would like to provide multiple images for responsiveness                                                                                                                                                    | `string`             | `undefined` |
| `imgWidth`           | `img-width`            | The image width                                                                                                                                                                                                                                                              | `number`             | `undefined` |
| `intrinsicsize`      | `intrinsicsize`        | An intrinsicsize for the native lazy-loading                                                                                                                                                                                                                                 | `string`             | `undefined` |
| `loading`            | `loading`              | If set to lazy, the web native lazy capability of the browser, if available, will be used to lazy load the image                                                                                                                                                             | `"eager" \| "lazy"`  | `'eager'`   |
| `observerRootMargin` | `observer-root-margin` | A string which specifies a set of offsets to add to the root's bounding_box when calculating intersections, effectively shrinking or growing the root for calculation purposes.                                                                                              | `string`             | `'300px'`   |
| `observerThreshold`  | `observer-threshold`   | Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the observed target.                                                                                                               | `number \| number[]` | `0.25`      |
| `svgSrc`             | `svg-src`              | The SVG image source (= URI) to lazy load and to parse (no <img/> tag will be use to render the svg) aria-label	string                                                                                                                                                       | `string`             | `undefined` |


## Events

| Event             | Description                                                                                                           | Type                                |
| ----------------- | --------------------------------------------------------------------------------------------------------------------- | ----------------------------------- |
| `customLoad`      | Emitted if component property custom-loader is set to true and if an image (img-src or img-src-set) has to be loaded. | `CustomEvent<DeckDeckGoCustomLoad>` |
| `innerImgDidLoad` | An event emitted when the shadowed image has loaded                                                                   | `CustomEvent<any>`                  |
| `lazyImgDidLoad`  | An event emitted after initialization when the component did load                                                     | `CustomEvent<any>`                  |


## Methods

### `lazyLoad() => Promise<void>`

This component also export an async method lazyLoad() in case you would like to trigger "manually" the loading of the image

#### Returns

Type: `Promise<void>`




## Shadow Parts

| Part    | Description                               |
| ------- | ----------------------------------------- |
| `"img"` | A CSS :part to access the slotted <img /> |


## CSS Custom Properties

| Name                                   | Description                                                                                                     |
| -------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `--deckgo-lazy-img-border-radius`      | In case you would like to specify a border radius for the image                                                 |
| `--deckgo-lazy-img-box-shadow`         | Image box-shadow                                                                                                |
| `--deckgo-lazy-img-display`            | The display property of the image                                                                               |
| `--deckgo-lazy-img-float`              | Image float                                                                                                     |
| `--deckgo-lazy-img-height`             | Image max height                                                                                                |
| `--deckgo-lazy-img-margin`             | Image margin                                                                                                    |
| `--deckgo-lazy-img-max-height`         | Image max height                                                                                                |
| `--deckgo-lazy-img-max-width`          | Image max width @default 100%                                                                                   |
| `--deckgo-lazy-img-min-height`         | Image min height                                                                                                |
| `--deckgo-lazy-img-min-width`          | Image min width                                                                                                 |
| `--deckgo-lazy-img-object-fit`         | The property object-fit of the image                                                                            |
| `--deckgo-lazy-img-opacity-loaded`     | The opacity of the image when loaded @default 1                                                                 |
| `--deckgo-lazy-img-opacity-not-loaded` | The opacity of the image when not loaded @default 0                                                             |
| `--deckgo-lazy-img-padding`            | Image padding                                                                                                   |
| `--deckgo-lazy-img-pointer-events`     | Image pointer events @default none                                                                              |
| `--deckgo-lazy-img-transition`         | The animation of the image, notably use to display smoothly the image when loaded @default opacity 0.15s linear |
| `--deckgo-lazy-img-vertical-align`     | Image vertical alignment                                                                                        |
| `--deckgo-lazy-img-width`              | Image max width                                                                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
