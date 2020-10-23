# Slide: Split

The "Split" slide is a simple slide which display two panes on the page.

## Table of contents

- [Layout](#app-slide-split-layout)
  - [Horizontal](#app-slide-split-layout-horizontal)
  - [Vertical](#app-slide-split-layout-vertical)
- [Installation](#app-slide-split-installation)
  - [From a CDN](#app-slide-split-from-a-cdn)
  - [From NPM](#app-slide-split-from-npm)
  - [Framework integration](#app-slide-split-framework-integration)
- [Usage](#app-slide-split-usage)
  - [Slots](#app-slide-split-slots)
- [Attributes](#app-slide-split-attributes)
- [Theming](#app-slide-split-theming)

## Layout

This template could split the content in two different ways.

### Horizontal

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-split>
        <p slot="start">
          The content you want to display on the left side of the page
        </p>
        <p slot="end">
          The content you want to display on the right side of the page
        </p>
      </deckgo-slide-split>
  </deckgo-deck>
</div>

### Vertical

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-split vertical={true}>
        <p slot="start">
          The content you want to display on the top of the page
        </p>
        <p slot="end">
          The content you want to display on the bottom of the page
        </p>
      </deckgo-slide-split>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-split@latest/dist/deckdeckgo-slide-split/deckdeckgo-slide-split.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-split) run the following command:

```bash
npm install @deckdeckgo/slide-split
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-split';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-split/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Split" slide's Web Component could be integrated using the tag `<deckgo-slide-split/>`.

```
<deckgo-deck>
  <deckgo-slide-split>
    <h1 slot="title">Two columns subject</h1>
    <p slot="start">
      The content you want to display on the left side of the page
    </p>
    <p slot="end">
      The content you want to display on the right side of the page
    </p>
  </deckgo-slide-split>
</deckgo-deck>
```

### Slots

Both slots `title`, `start` and `end` are optional. Without providing one of them, the page will remain empty.

The `start` slot is the content of the left pane respectively the slot `end` is the content of the right pane.

Note: The slot `title` is per default hidden even if you provide it. See attributes below if you wish to display it.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default | Description                                                                                                                                    |
| ----------------- | ------- | ------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| vertical          | boolean | false   | Per default this template is horizontally split (two columns). Turn this property to `true` too display two rows respectively split vertically |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true`                                |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`                                     |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note                                                                                                             |
| ---------------------------------- | ------- | ---------------------------------------------------------------------------------------------------------------- |
| --background                       |         |                                                                                                                  |
| --color                            |         |                                                                                                                  |
| --slide-split-background-start     |         | Left split pane background                                                                                       |
| --slide-split-color-start          |         | Left split pane color                                                                                            |
| --slide-split-background-end       |         | Right split pane background                                                                                      |
| --slide-split-color-end            |         | Right split pane color                                                                                           |
| --slide-split-padding-top          | 0       | Padding top of a slide split pane                                                                                |
| --slide-split-padding-end          | 64px    | Padding right of a slide split pane                                                                              |
| --slide-split-padding-bottom       | 0       | Padding bottom of a slide split pane                                                                             |
| --slide-split-padding-start        | 64px    | Padding left of a slide split pane                                                                               |
| --slide-split-title-padding-top    | 16px    | Padding top of the title of the                                                                                  |
| --slide-split-title-padding-end    | 32px    | Padding right of the title of the                                                                                |
| --slide-split-title-padding-bottom | 16px    | Padding bottom of the title of the                                                                               |
| --slide-split-title-padding-start  | 32px    | Padding left of the title of the                                                                                 |
| --slide-padding-start              | 32px    | Modify slotted ul and ol padding-inline-start                                                                    |
| --slide-split-align                | inherit | Modify for example to center if you want to align the content in the middle                                      |
| --slide-split-text-align           | inherit | Modify for example to center if you want to align the text in the middle                                         |
| --slide-split-title-display        | none    | The `slot` title is per default hidden even if you provide it. If you wish to displays it, modify this attribute |
| --zIndex                           | 1       | The z-index of the slide                                                                                         |
| --slide-split-display-start        | flex    | Start side display property                                                                                      |
| --slide-split-display-end          | flex    | End side display property                                                                                        |

[deckdeckgo]: https://deckdeckgo.com
