# Slide: Gif

The "Gif" slide let you add easily a gif, like those provided by [Giphy](https://giphy.com), to your presentation.

## Table of contents

- [Layout](#app-slide-gif-layout)
- [Installation](#app-slide-gif-installation)
  - [From a CDN](#app-slide-gif-from-a-cdn)
  - [From NPM](#app-slide-gif-from-npm)
  - [Framework integration](#app-slide-gif-framework-integration)
- [For images too](#app-slide-gif-for-images-too)
- [Usage](#app-slide-gif-usage)
  - [Slots](#app-slide-gif-slots)
  - [Notes](#app-slide-gif-notes)
- [Gif component](#app-slide-gif-gif-component)
- [Attributes](#app-slide-gif-attributes)
- [Theming](#app-slide-gif-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
      <h1 slot="title">My title</h1>
      <h1 slot="header">Hey</h1>
      <h2 slot="footer">It's a cool gif</h2>
    </deckgo-slide-gif>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.
 
### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-gif@latest/dist/deckdeckgo-slide-gif/deckdeckgo-slide-gif.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/slide-gif@latest/dist/deckdeckgo-slide-gif/deckdeckgo-slide-gif.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/core) run the following command:

```bash
npm install @deckdeckgo/slide-gif
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-gif';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-gif/dist/loader';
deckDeckGoSlideElement(window);
```

## For images too

The slide Gif is useful for Gifs but could be use for any images too, in case you would like for example to display an image fullscreen.

## Usage

The "Gif" slide's Web Component could be integrated using the tag `<deckgo-slide-gif/>`.

```
<deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen="true">
  <h1 slot="title">My title</h1>
  <h1 slot="header">Hey</h1>
  <h2 slot="footer">It's a cool gif</h2>
</deckgo-slide-gif>
```

### Slots

The slots `title`, `header` and `footer` are both optional. `header` and `footer` would be displayed over the gif.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Gif component

The slide "Gif" relies on the component `<deckgo-gif/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| src | string |  | The source url, the src, of the Gif. Could be an embeddable external url or a local one. |
| alt | string |  | And alt information could be provided for accessibility reason. |
| fullscreen | number | true | If set to true, the gif width and height will be related to the slide width and height respectively will be fullscreen. |
| custom-background | boolean | false | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions | boolean | false | If you would provide actions for the all deck and a specific one for this slide, set this option to `true` |

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
| --zIndex | 1 | The z-index of the slide |

## Video

Have a look at this video where we demonstrate how to use it!

<iframe width="560" height="315" src="https://www.youtube.com/watch?v=0X3k3-yP7-Q" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

[DeckDeckGo]: https://deckdeckgo.com