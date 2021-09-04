# deckgo-slide-chart

The "Chart" slide let you draw easily charts in your presentation.


## Video

Have a look at this video where we demonstrate how to use it!

<iframe width="560" height="315" src="https://www.youtube.com/embed/ESD-K7zZT-c" frameborder="0"></iframe>

## Installation

This template could be added to your presentation using the following methods.

> This template is included per default in our Developer Kit

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

## Properties and Styling

This template supports the same attributes and CSS variables as the charts Web Components.

<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description                                                                                             | Type       | Default                        |
| ------------------- | -------------------- | ------------------------------------------------------------------------------------------------------- | ---------- | ------------------------------ |
| `animation`         | `animation`          |                                                                                                         | `boolean`  | `false`                        |
| `animationDuration` | `animation-duration` |                                                                                                         | `number`   | `1000`                         |
| `area`              | `area`               |                                                                                                         | `string`   | `undefined`                    |
| `customActions`     | `custom-actions`     | If you provide actions for the all deck but, a specific one for this slide, set this option to true     | `boolean`  | `false`                        |
| `customBackground`  | `custom-background`  | If you define a background for the all deck but, a specific one for this slide, set this option to true | `boolean`  | `false`                        |
| `customLoader`      | `custom-loader`      |                                                                                                         | `boolean`  | `false`                        |
| `datePattern`       | `date-pattern`       |                                                                                                         | `string`   | `undefined`                    |
| `grid`              | `grid`               |                                                                                                         | `string`   | `undefined`                    |
| `height`            | `height`             |                                                                                                         | `number`   | `undefined`                    |
| `innerRadius`       | `inner-radius`       |                                                                                                         | `number`   | `undefined`                    |
| `marginBottom`      | `margin-bottom`      |                                                                                                         | `number`   | `64`                           |
| `marginLeft`        | `margin-left`        |                                                                                                         | `number`   | `32`                           |
| `marginRight`       | `margin-right`       |                                                                                                         | `number`   | `32`                           |
| `marginTop`         | `margin-top`         |                                                                                                         | `number`   | `8`                            |
| `range`             | --                   |                                                                                                         | `string[]` | `undefined`                    |
| `separator`         | `separator`          |                                                                                                         | `string`   | `undefined`                    |
| `smooth`            | `smooth`             |                                                                                                         | `string`   | `undefined`                    |
| `src`               | `src`                |                                                                                                         | `string`   | `undefined`                    |
| `ticks`             | `ticks`              |                                                                                                         | `number`   | `undefined`                    |
| `type`              | `type`               | The type of the chart, pie, line or bar                                                                 | `string`   | `DeckdeckgoSlideChartType.PIE` |
| `width`             | `width`              |                                                                                                         | `number`   | `undefined`                    |
| `yAxisDomain`       | `y-axis-domain`      |                                                                                                         | `string`   | `undefined`                    |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(enter: boolean, _reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `draw() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `getRandomColors() => Promise<string[] | undefined | null>`

Returns the list of the random colors that have been generated.

#### Returns

Type: `Promise<string[]>`



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `postCustomLoad(content: string | undefined) => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot           | Description                        |
| -------------- | ---------------------------------- |
| `"actions"`    | Custom actions for this slide      |
| `"background"` | A custom background for this slide |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                     | Description                                                        |
| ------------------------ | ------------------------------------------------------------------ |
| `--background`           | background                                                         |
| `--color`                | color                                                              |
| `--overflow`             | overflow of the slide @default hidden                              |
| `--slide-chart-overflow` | overflow @default hidden                                           |
| `--slide-padding-bottom` | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-padding-end`    | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-padding-start`  | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-padding-top`    | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-user-select`    | user select @default none                                          |
| `--zIndex`               | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
