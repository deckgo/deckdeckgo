# Slide: Big Image

If you would like to display a fullscreen image in your presentation and select specific part of it, in order to to zoom in/highlight them, the "Big Image" slide is the one you are looking for.

## Table of contents

- [Layout](#app-slide-bigimg-layout)
- [Installation](#app-slide-bigimg-installation)
  - [From a CDN](#app-slide-bigimg-from-a-cdn)
  - [From NPM](#app-slide-bigimg-from-npm)
  - [Framework integration](#app-slide-bigimg-framework-integration)
- [Usage](#app-slide-bigimg-usage)
  - [Slots](#app-slide-bigimg-slots)
- [Attributes](#app-slide-bigimg-attributes)
- [Theming](#app-slide-bigimg-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-big-img
        img-src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/big-img/showcase/big-deckdeckgo.jpg"
        img-divisions="500;1100;1700"
        axis="y"
    >
    </deckgo-slide-big-img>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-big-img@latest/dist/deckdeckgo-slide-big-img/deckdeckgo-slide-big-img.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-big-img) run the following command:

```bash
npm install @deckdeckgo/slide-big-img
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-big-img';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-big-img/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Big Image" slide's Web Component could be integrated using the tag `<deckgo-slide-big-img/>`.

```
<deckgo-slide-big-img
     img-src="https://raw.githubusercontent.com/noelmace/deckdeckgo/big-img/webcomponents/slides/big-img/showcase/big-deckdeckgo.jpg"
     img-divisions="500;1100;1700"
     axis="y">
</deckgo-slide-big-img>
```

### Slots

The slots `title` is optional (it is not displayed but could be use for the navigation). Otherwise no particular slots are currently available in order to display additional information on this template.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute      | Type       | Default | Description                                                                                                                   |
| -------------- | ---------- | ------- | ----------------------------------------------------------------------------------------------------------------------------- |
| img-src        | string     |         | An image URI which should be first displayed full screen before being divided in separate parts presented on the next slides. |
| img-alt        | string     |         | An optional accessibility alt for the image.                                                                                  |
| img-divisions  | string     |         | A list of anchors for the divisions of the image (in pixels).                                                                 |
| axis           | 'x' or 'y' |         | The axis which should be used to apply the division.                                                                          |
| reverse        | boolean    | false   | In which order should the specific part be highlighted.                                                                       |
| custom-actions | boolean    | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`                    |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable          | Default | Note                                                                                                                                        |
| ---------------------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| --background           |         |                                                                                                                                             |
| --color                |         |                                                                                                                                             |
| --slide-padding-top    | 16px    | Padding top of the all slide                                                                                                                |
| --slide-padding-end    | 32px    | Padding right of the all slide                                                                                                              |
| --slide-padding-bottom | 16px    | Padding bottom of the all slide                                                                                                             |
| --slide-padding-start  | 32px    | Padding left of the all slide                                                                                                               |
| --zIndex               | 1       | The z-index of the slide                                                                                                                    |
| --slide-img-max-width  |         | A maximal width value for the image. Useful in case you would like to display your deck in a container respectively not full window/screen. |

[deckdeckgo]: https://deckdeckgo.com
