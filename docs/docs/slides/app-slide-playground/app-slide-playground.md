# Slide: Playground

The "Playground" template helps embed easily playgrounds as [Codepen](https://codepen.io), [JSFiddle](https://jsfiddle.net/) and [WebComponents.dev](https://webcomponents.dev) in your presentation.

## Table of contents

- [Layout](#app-slide-playground-layout)
- [Installation](#app-slide-playground-installation)
  - [From a CDN](#app-slide-playground-from-a-cdn)
  - [From NPM](#app-slide-playground-from-npm)
  - [Framework integration](#app-slide-playground-framework-integration)
  - [YouTube component](#app-slide-playground-youtube-component)
- [Usage](#app-slide-playground-usage)
  - [Slots](#app-slide-playground-slots)
  - [YouTube component](#app-slide-playground-youtube-component)
- [Attributes](#app-slide-playground-attributes)
- [Theming](#app-slide-playground-theming)
- [Methods](#app-slide-playground-methods)
  - [Play the video](#app-slide-playground-play-the-video)
  - [Pause the video](#app-slide-playground-pause-the-video)
  - [Toggle the video](#app-slide-playground-toggle-the-video)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-playground src="https://codepen.io/peterpeterparker/pen/dyGbOZm">
      <h1 slot="title">My Codepen</h1>
    </deckgo-slide-playground>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-playground@latest/dist/deckdeckgo-slide-playground/deckdeckgo-slide-playground.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/slide-playground@latest/dist/deckdeckgo-slide-playground/deckdeckgo-slide-playground.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-playground) run the following command:

```bash
npm install @deckdeckgo/slide-playground
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-playground';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-playground/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Playground" slide's Web Component could be integrated using the tag `<deckgo-slide-playground/>`.

```
<deckgo-slide-playground src="https://codepen.io/peterpeterparker/pen/dyGbOZm">
  <h1 slot="title">My Codepen</h1>
</deckgo-slide-playground>
```

### Slots

Both slots `title` and `content` are optional.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type                           | Default                                                                                    | Description                                                                                                                         |
| ----------------- | ------------------------------ | ------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------- |
| src               | string                         |                                                                                            | The full link to your Pen, Fiddle oder WebComponents.dev. The component will take care of converting the link to an embeddable one. |
| theme             | 'default' or 'light' or 'dark' | 'default                                                                                   | The theming option if it can be applied respectivelly if supported by the third party playground, otherwise, 'default'.             |
| width             | number                         | Per default the playground width will be calculated according the content size available.  | Using this option you would be able to define your own width.                                                                       |
| height            | number                         | Per default the playground height will be calculated according the content size available. | Using this option you would be able to define your own height.                                                                      |
| custom-background | boolean                        | false                                                                                      | If you would provide a background for the all deck and a specific one for this slide, set this option to `true`                     |
| custom-actions    | boolean                        | false                                                                                      | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`                          |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable               | Default           | Note                                       |
| --------------------------- | ----------------- | ------------------------------------------ |
| --background                |                   |                                            |
| --color                     |                   |                                            |
| --slide-padding-top         | 16px              | Padding top of the all slide               |
| --slide-padding-end         | 32px              | Padding right of the all slide             |
| --slide-padding-bottom      | 16px              | Padding bottom of the all slide            |
| --slide-padding-start       | 32px              | Padding left of the all slide              |
| --zIndex                    | 1                 | The z-index of the slide                   |
| --slide-playground-margin   | 32px 0 32px       | The margin of the playground's container   |
| --slide-playground-height   | calc(100% - 32px) | The height of the playground's container   |
| --slide-playground-overflow | auto              | The overflow of the playground's container |

[deckdeckgo]: https://deckdeckgo.com
