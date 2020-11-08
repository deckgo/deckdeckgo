# Lazy Image

The "Lazy Image" component is a dead simple component to lazy load images.

It leverages the native lazy-loading and the IntersectionObserver API (default behavior) to lazy load images. It also allows you to trigger "manually" their loading.

An `<img/>` tag is per default use to display the image but optionally it could parse `SVG` too.

## Table of contents

- [Installation](#app-components-lazy-img-installation) - [Using from a CDN](#app-components-lazy-img-from-a-cdn) - [Install from NPM](#app-components-lazy-img-from-npm) - [Framework integration](#app-components-lazy-img-framework-integration)
- [Usage](#app-components-lazy-img-usage)
  - [Slots](#app-components-lazy-img-slots)
  - [Attributes](#app-components-lazy-img-attributes)
  - [Theming](#app-components-lazy-img-theming)
  - [Methods](#app-components-lazy-img-methods)
  - [Events](#app-components-lazy-img-events)
- [Fallbacks]((#app-components-lazy-img-fallbacks)
- [Trying it out](#app-components-lazy-img-trying-it-out)

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

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

### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute            | Type               | Default                                                                                                                                                                                                                                                                                | Description                                                                                                                                                                                                                                                                              |
| -------------------- | ------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| img-src              | string             |                                                                                                                                                                                                                                                                                        | The image source (= URI) to lazy load                                                                                                                                                                                                                                                    |
| img-src-set          | string             |                                                                                                                                                                                                                                                                                        | The attribute "srcset" (= multiple URI) to lazy load in case you would like to provide multiple images for responsiveness                                                                                                                                                                |
| img-error-src        | string             |                                                                                                                                                                                                                                                                                        | An optional image which could be displayed in case the main image would not be resolved                                                                                                                                                                                                  |
| img-sizes            | string             |                                                                                                                                                                                                                                                                                        | The set of media conditions to indicates what image size would be best to choose                                                                                                                                                                                                         |
| img-alt              | string             |                                                                                                                                                                                                                                                                                        | The image alternate text                                                                                                                                                                                                                                                                 |
| img-width            | number             |                                                                                                                                                                                                                                                                                        | The image width                                                                                                                                                                                                                                                                          |
| img-height           | number             |                                                                                                                                                                                                                                                                                        | The image height                                                                                                                                                                                                                                                                         |
| svg-src              | string             |                                                                                                                                                                                                                                                                                        | The SVG image source (= URI) to lazy load and to parse (no `<img/>` tag will be use to render the svg)                                                                                                                                                                                   |
| aria-label           | string             |                                                                                                                                                                                                                                                                                        | If you are using the above SVG option, provide the accessibility information using this attribute                                                                                                                                                                                        |
| observer-root-margin | string             | 100px 0px                                                                                                                                                                                                                                                                              | A string which specifies a set of offsets to add to the root's bounding_box when calculating intersections, effectively shrinking or growing the root for calculation purposes. [More info.](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver) |
| observer-threshold   | number or number[] |                                                                                                                                                                                                                                                                                        | Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the observed target. [More info.](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver)                  |
| intrinsicsize        | string             | An intrinsicsize for the native lazy-loading (see [Native lazy-loading for the web](https://web.dev/native-lazy-loading))                                                                                                                                                              |                                                                                                                                                                                                                                                                                          |
| custom-loader        | boolean            | In case you would like to take care by yourself to apply the load of the image. If turn to `true` then the component will emit an event `customLoad` when the image intersect the viewport instead of displaying it (doesn't apply for `svg` but only for `img-src` and `img-src-set`) |                                                                                                                                                                                                                                                                                          |
| loading              | `eager` or `lazy`  | Per default `eager` because the intersection observer is used to defer the loading.                                                                                                                                                                                                    | If set to `lazy`, the web native lazy capability of the browser, if available, will be used to lazy load the image                                                                                                                                                                       |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                        | Default              | Note                                                                              |
| ------------------------------------ | -------------------- | --------------------------------------------------------------------------------- |
| --deckgo-lazy-img-max-height         |                      | Image max height                                                                  |
| --deckgo-lazy-img-max-width          | 100%                 | Image max width                                                                   |
| --deckgo-lazy-img-min-height         |                      | Image min height                                                                  |
| --deckgo-lazy-img-min-width          |                      | Image min width                                                                   |
| --deckgo-lazy-img-pointer-events     | none                 | Image pointer events                                                              |
| --deckgo-lazy-img-height             |                      | Image height                                                                      |
| --deckgo-lazy-img-width              |                      | Image width                                                                       |
| --deckgo-lazy-img-float              |                      | Image float                                                                       |
| --deckgo-lazy-img-padding            |                      | Image padding                                                                     |
| --deckgo-lazy-img-vertical-align     |                      | Image vertical alignment                                                          |
| --deckgo-lazy-img-display            |                      | The display property of the image                                                 |
| --deckgo-lazy-img-border-radius      |                      | In case you would like to specify a border radius for the image                   |
| --deckgo-lazy-img-object-fit         |                      | The property object-fit of the image                                              |
| --deckgo-lazy-img-opacity-not-loaded | 0                    | The opacity of the image when not loaded                                          |
| --deckgo-lazy-img-opacity-loaded     | 1                    | The opacity of the image when loaded                                              |
| --deckgo-lazy-img-transition         | opacity 0.15s linear | The animation of the image, notably use to display smoothly the image when loaded |
| --deckgo-lazy-img-box-shadow         |                      | Image box-shadow                                                                  |

### Methods

This component also export an async method `lazyLoad()` in case you would like to trigger "manually" the loading of the image.

```
const element = document.querySelector('deckgo-lazy-img');
await element.lazyLoad();
```

### Events

The `<deckgo-lazy-img/>` will bubble the following events:

| Event        | Description                                                                                                                  | Type                                |
| ------------ | ---------------------------------------------------------------------------------------------------------------------------- | ----------------------------------- |
| `customLoad` | Emitted if component property `custom-loader` is set to `true` and if an image (`img-src` or `img-src-set`) as to be loaded. | `CustomEvent<DeckDeckGoCustomLoad>` |

Where `DeckDeckGoCustomLoad` contains the shadowed image element and both information `img-src` and `img-src-set`.

### Fallbacks

In case the browser would not support the native native lazy-loading or the Intersection Observer API, images are going to be loaded without any delay when the component load respectively if the browser does not implement the Intersection Observer API images are displayed and not lazy loaded.

### Trying it out

This component lazy load images when these are not presented in the viewport. If you would use this component in a simple test containing only a couple of images, respectively no content or no real use case where the images are effectively offscreen, assign a default height to components in order to ensure that some are effectively placed outside of the window [[#128]](https://github.com/deckgo/deckdeckgo/issues/128#issuecomment-493979841).

[deckdeckgo]: https://deckdeckgo.com
