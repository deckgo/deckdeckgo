# Slide: Chart

The "Chart" slide let you draw easily charts in your presentation.

## Table of contents

- [Layout](#app-slide-chart-layout)
- [Video](#app-slide-chart-video)
- [Installation](#app-slide-chart-installation)
  - [From a CDN](#app-slide-chart-from-a-cdn)
  - [From NPM](#app-slide-chart-from-npm)
  - [Framework integration](#app-slide-chart-framework-integration)
- [Usage](#app-slide-chart-usage)
  - [Slots](#app-slide-chart-slots)
- [Chart components](#app-slide-chart-chart-components)
- [Installation](#app-slide-chart-installation)
- [Attributes](#app-slide-chart-attributes)
- [Theming](#app-slide-chart-theming)
- [Methods](#app-slide-chart-methods)
  - [Draw](#app-slide-chart-draw)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

## Video

Have a look at this video where we demonstrate how to use it!

<iframe width="560" height="315" src="https://www.youtube.com/embed/ESD-K7zZT-c" frameborder="0"></iframe>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-chart@latest/dist/deckdeckgo-slide-chart/deckdeckgo-slide-chart.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-chart) run the following command:

```bash
npm install @deckdeckgo/slide-chart
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-chart';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-chart/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Chart" slide's Web Component could be integrated using the tag `<deckgo-slide-chart/>`.

```
<deckgo-slide-chart src="./assets/csv/data-pie-chart.csv">
    <h1 slot="title">My Pie chart</h1>
</deckgo-slide-chart>
```

### Slots

The slot `title` is optional.

## Chart components

The slide "Chart" relies on the charts components `<deckgo-pie-chart/>`, `<deckgo-line-chart/>` and `<deckgo-bar-chart/>` which are described in the components [documentation](/components/charts).

## Installation

The [DeckDeckGo] charts components are provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] chart to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/@deckdeckgo/charts) as described in its [installation guide](https://docs.deckdeckgo.com/components/charts).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default | Description                                                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| type              | string  | pie     | The type of the chart, `pie`, `line` or `bar`                                                                   |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

Furthermore, this slide component offers the same attributes as the [DeckDeckGo] charts Web Component, see its [documentation](/components/charts) for the details.

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable          | Default | Note                            |
| ---------------------- | ------- | ------------------------------- |
| --background           |         |                                 |
| --color                |         |                                 |
| --slide-padding-top    | 16px    | Padding top of the all slide    |
| --slide-padding-end    | 32px    | Padding right of the all slide  |
| --slide-padding-bottom | 16px    | Padding bottom of the all slide |
| --slide-padding-start  | 32px    | Padding left of the all slide   |
| --zIndex               | 1       | The z-index of the slide        |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] charts Web Component, see its [documentation](/components/charts) for the details.

## Methods

The slide "Chart" exposes the following methods:

### Draw

In case you would like to draw or redraw your chart.

```
const slide = deck.getElementsByTagName('deckgo-slide-chart');
await slide.draw();
```

[deckdeckgo]: https://deckdeckgo.com
