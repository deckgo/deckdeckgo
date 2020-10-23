# Slide: Aspect Ratio

The "Aspect Ratio" slide is a template which preserves the content ratio regardless of the devices.

We use this slide in the [DeckDeckGo] editor to let users create slides containing shapes, for example to create schema.

## Table of contents

- [Layout](#app-slide-aspect-ratio-layout)
- [Installation](#app-slide-aspect-ratio-installation)
  - [From a CDN](#app-slide-aspect-ratio-from-a-cdn)
  - [From NPM](#app-slide-aspect-ratio-from-npm)
  - [Framework integration](#app-slide-aspect-ratio-framework-integration)
- [Usage](#app-slide-aspect-ratio-usage)
  - [Slots](#app-slide-aspect-ratio-slots)
- [Attributes](#app-slide-aspect-ratio-attributes)
- [Example](#app-slide-aspect-ratio-example)
- [Theming](#app-slide-aspect-ratio-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-aspect-ratio grid={true}>
        <h1 style={{position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)', margin: '0'}}>Any elements</h1>
      </deckgo-slide-aspect-ratio>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-aspect-ratio@latest/dist/deckdeckgo-slide-aspect-ratio/deckdeckgo-slide-aspect-ratio.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-aspect-ratio) run the following command:

```bash
npm install @deckdeckgo/slide-aspect-ratio
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-aspect-ratio';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-aspect-ratio/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Aspect Ratio" slide's Web Component could be integrated using the tag `<deckgo-slide-aspect-ratio/>`.

```
<deckgo-deck>
  <deckgo-slide-aspect-ratio>
    <h1 style="position: absolute; top: 50%; left: 25%">An element</h1>
    <p style="position: absolute; top: 4%; left: 5%">
      Another element
    </p>
  </deckgo-slide-aspect-ratio>
</deckgo-deck>
```

### Slots

The slots `title`, `top` and `bottom` are both optional. `top` and `bottom` would be displayed over the content.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default | Description                                                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| ratio             | number  | 16 / 9  | The aspect ratio of the displayed content. Per default 16 being the width and 9 the height                      |
| grid              | boolean | false   | Display a grid behind the content. Note that the grid would only be display if not fullscreen                   |
| editable          | boolean | false   | Per default `point-events` are set to `none` for this template making it read-only respectively not editable    |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

## Example

```
<deckgo-deck embedded={true}>
  <deckgo-slide-aspect-ratio grid={true}>
    <h1 style={{position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)', margin: '0'}}>Any elements</h1>
  </deckgo-slide-aspect-ratio>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                | Default                                                                                                                                                       | Note                                         |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------- |
| --slide-grid-background      | linear-gradient(to bottom, rgba(0, 0, 0, 0) 98%, rgba(110, 109, 111, 0.4) 98%), linear-gradient(to right, rgba(0, 0, 0, 0) 98%, rgba(110, 109, 111, 0.4) 98%) | The default grid color                       |
| --slide-grid-background-size | 2em 2em                                                                                                                                                       | The default size of each squares of the grid |
| --background                 |                                                                                                                                                               |                                              |
| --color                      |                                                                                                                                                               |                                              |
| --slide-padding-top          | 16px                                                                                                                                                          | Padding top of the all slide                 |
| --slide-padding-end          | 32px                                                                                                                                                          | Padding right of the all slide               |
| --slide-padding-bottom       | 16px                                                                                                                                                          | Padding bottom of the all slide              |
| --slide-padding-start        | 32px                                                                                                                                                          | Padding left of the all slide                |
| --zIndex                     | 1                                                                                                                                                             | The z-index of the slide                     |

[deckdeckgo]: https://deckdeckgo.com
