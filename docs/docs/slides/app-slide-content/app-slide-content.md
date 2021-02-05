# Slide: Content

The "Content" slide is a simple slide which display its title and content aligned to the start of the page.

This slide could be for example use for the every slides of your presentation where you would like to display content related to your talk.

## Table of contents

- [Layout](#app-slide-content-layout)
- [Installation](#app-slide-content-installation)
  - [From a CDN](#app-slide-content-from-a-cdn)
  - [From NPM](#app-slide-content-from-npm)
  - [Framework integration](#app-slide-content-framework-integration)
- [Usage](#app-slide-content-usage)
  - [Slots](#app-slide-content-slots)
- [Attributes](#app-slide-content-attributes)
  - [Example](#app-slide-content-example)
- [Theming](#app-slide-content-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-content>
        <h1 slot="title">Something related to my topic</h1>
        <p slot="content">
          Cool beans
        </p>
      </deckgo-slide-content>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-content@latest/dist/deckdeckgo-slide-content/deckdeckgo-slide-content.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-content) run the following command:

```bash
npm install @deckdeckgo/slide-content
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-content';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-content/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Content" slide's Web Component could be integrated using the tag `<deckgo-slide-content/>`.

```
<deckgo-deck>
  <deckgo-slide-content>
    <h1 slot="title">Something related to my topic</h1>
    <p slot="content">
      Cool beans
    </p>
  </deckgo-slide-content>
</deckgo-deck>
```

### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default | Description                                                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

### Example

```
<deckgo-deck>
  <deckgo-slide-content>
    <h1 slot="title">Something related to my topic</h1>
    <ul slot="content">
      <li>Cool</li>
      <li>Beans</li>
    </ul>
  </deckgo-slide-content>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                   | Default    | Note                             |
| ------------------------------- | ---------- | -------------------------------- |
| --background                    |            |                                  |
| --color                         |            |                                  |
| --slide-padding-top             | 16px       | Padding top of the all slide     |
| --slide-padding-end             | 32px       | Padding right of the all slide   |
| --slide-padding-bottom          | 16px       | Padding bottom of the all slide  |
| --slide-padding-start           | 32px       | Padding left of the all slide    |
| --slide-content-justify-content | flex-start | Justify the content of the slide |
| --zIndex                        | 1          | The z-index of the slide         |

[deckdeckgo]: https://deckdeckgo.com
