# deckgo-pie-chart

Draw pie charts easily.

To create and draw the charts, this project is using [D3js](https://d3js.org).

## Installation

This component can be added to your web application with following methods.

> If you are using our developer kit to create a presention, this component is already included

### Using DeckDeckGo charts from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) charts from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/charts@latest/dist/deckdeckgo-charts/deckdeckgo-charts.esm.js"></script>
```

### Install DeckDeckGo charts from NPM

Install [DeckDeckGo](https://deckdeckgo.com) charts in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/charts) using the following command:

```bash
npm install @deckdeckgo/charts
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/charts';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/charts/dist/loader';
deckDeckGoElement();
```

## Usage

The main idea behind the [DeckDeckGo](https://deckdeckgo.com) charts is to be able to drop a **csv** file, provide it to the component and let it do the job respectively draw the chart.

### Pie usage

The `<deckgo-pie-chart/>` Web Component draw a Pie chart.

#### CSV

The csv file should contains at least two columns. The first one should be a label, which could be displayed or not, and the second one should be a value.

If more than two columns are provided, all columns beside the first one will be interpreted as values for comparison, in case you would like to displayed multiple graphs.

The values could be provided as `number` or `percent`.

##### Example

In this example, the first column contains a label for the category and the second contains the value in percent.

```
Wind;53.13%
Solar:1.96%
Geothermal;7.52%
Landfill Gas;15.67%
Biomass;14.79%
Qualified Hydropower;6.93%
```

<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description                                                                                                                     | Type      | Default     |
| ------------------- | -------------------- | ------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------- |
| `animation`         | `animation`          | Display multiple graphs and animate the transition between these                                                                | `boolean` | `false`     |
| `animationDuration` | `animation-duration` | Duration of the transition between graphs                                                                                       | `number`  | `1000`      |
| `customLoader`      | `custom-loader`      | Set to `true` in case you would like to load (fetch) the data by yourself. Useful in case your data are protected with a token. | `boolean` | `false`     |
| `height`            | `height`             | The height of the chart                                                                                                         | `number`  | `undefined` |
| `innerRadius`       | `inner-radius`       | The inner radius of the pie                                                                                                     | `number`  | `0`         |
| `marginBottom`      | `margin-bottom`      | The margin bottom of the chart in pixel                                                                                         | `number`  | `64`        |
| `marginLeft`        | `margin-left`        | The margin left of the chart in pixel                                                                                           | `number`  | `32`        |
| `marginRight`       | `margin-right`       | The margin right of the chart in pixel                                                                                          | `number`  | `32`        |
| `marginTop`         | `margin-top`         | The margin top of the chart in pixel                                                                                            | `number`  | `8`         |
| `separator`         | `separator`          | The line separator use in your csv file                                                                                         | `string`  | `';'`       |
| `src`               | `src`                | The path to the source file of the data                                                                                         | `string`  | `undefined` |
| `width`             | `width`              | The width of the chart                                                                                                          | `number`  | `undefined` |


## Events

| Event             | Description                                                                  | Type                  |
| ----------------- | ---------------------------------------------------------------------------- | --------------------- |
| `chartCustomLoad` | The event to be processed to load the data if you are using a custom loader. | `CustomEvent<string>` |


## Methods

### `draw(width?: number, height?: number) => Promise<void>`

In case you would like to redraw your chart, for example on resize of the window.

#### Returns

Type: `Promise<void>`



### `isBeginning() => Promise<boolean>`

Is animation at the begin of the serie.

#### Returns

Type: `Promise<boolean>`



### `isEnd() => Promise<boolean>`

Is animation at the end of the serie.

#### Returns

Type: `Promise<boolean>`



### `next() => Promise<void>`

If you are using animation, this method is used to display the next data respectively the next chart.

#### Returns

Type: `Promise<void>`



### `postCustomLoad(content: string | undefined) => Promise<void>`



#### Returns

Type: `Promise<void>`



### `prev() => Promise<void>`

If you are using animation, this method is used to display the previous data respectively the previous chart.

#### Returns

Type: `Promise<void>`




## CSS Custom Properties

| Name                              | Description                                                                             |
| --------------------------------- | --------------------------------------------------------------------------------------- |
| `--deckgo-chart-display`          | The display property of the chart host @default block                                   |
| `--deckgo-chart-margin`           | The margin of the chart host @default 0                                                 |
| `--deckgo-chart-path-stroke`      | The color of the path                                                                   |
| `--deckgo-chart-position`         | The position of the chart host @default relative                                        |
| `--deckgo-chart-svg-left`         | The left position of the svg generated by the chart @default 50%                        |
| `--deckgo-chart-svg-overflow`     | The overflow property of the svg generated by the chart                                 |
| `--deckgo-chart-svg-position`     | The position of the svg generated by the chart @default absolute                        |
| `--deckgo-chart-svg-top`          | The top position of the svg generated by the chart @default 50%                         |
| `--deckgo-chart-svg-translate`    | The translate property of the svg generated by the chart @default translate(-50%, -50%) |
| `--deckgo-chart-text-color`       | The color of the labels @default black                                                  |
| `--deckgo-chart-text-display`     | The display property of the labels                                                      |
| `--deckgo-chart-text-font-family` | The font family of the labels of the chart @default initial                             |
| `--deckgo-chart-text-font-size`   | The font size of the labels of the chart                                                |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
