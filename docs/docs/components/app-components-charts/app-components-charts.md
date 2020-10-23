# Charts

The "Charts" component is an extra component which let you draw charts easily.

To create and draw the charts, this project is using [D3js](https://d3js.org).

## Table of contents

- [Showcase](#app-components-charts-showcase) - [Pie](#app-components-charts-pie) - [Donut](#app-components-charts-donut) - [Animated pie](#app-components-charts-animated-pie) - [Compare two graphs](#app-components-charts-compare-two-graphs) - [Line and area](#app-components-charts-line-and-area) - [Compare two lines and smoothing effect](#app-components-charts-compare-two-lines-and-smoothing-effect) - [Compare two lines and display a grid](#app-components-charts-compare-two-lines-and-display-a-grid) - [Animated lines](#app-components-charts-animated-lines) - [Bar](#app-components-charts-bar) - [Compare multiple bars](#app-components-charts-compare-multiple-bars) - [Animated bars](#app-components-charts-animated-bars) - [Video](#app-components-charts-video)
- [Installation](#app-components-charts-installation) - [Using DeckDeckGo charts from a CDN](#app-components-charts-using-deckdeckgo-charts-from-a-cdn) - [Install DeckDeckGo charts from NPM](#app-components-charts-install-deckdeckgo-charts-from-npm) - [Framework integration](#app-components-charts-framework-integration)
- [Usage](#app-components-charts-usage) - [Pie usage](#app-components-charts-pie-usage) - [CSV](#app-components-charts-csv) - [Example](#app-components-charts-example) - [Properties](#app-components-charts-properties) - [Styling](#app-components-charts-styling) - [Methods](#app-components-charts-methods) - [Examples](#app-components-charts-examples) - [Line usage](#app-components-charts-line-usage) - [CSV](#app-components-charts-csv-1) - [Two columns](#app-components-charts-two-columns-1) - [Three columns or more](#app-components-charts-three-columns-or-more) - [Properties](#app-components-charts-properties-1) - [Styling](#app-components-charts-styling-1) - [Methods](#app-components-charts-methods-1) - [Examples](#app-components-charts-examples-1) - [Bar usage](#app-components-charts-bar-usage) - [CSV](#app-components-charts-csv-2) - [Multiple columns](#app-components-charts-multiple-columns) - [Properties](#app-components-charts-properties-2) - [Styling](#app-components-charts-styling-2) - [Methods](#app-components-charts-methods-2) - [Examples](#app-components-charts-examples-2)

## Showcase

The following examples are the one provided in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html) of this component. If you would like to run them locally, proceed as follow:

```
git clone https://github.com/deckgo/deckdeckgo
cd webcomponents/charts
npm install
npm run start
```

### Pie

<deckgo-pie-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"></deckgo-pie-chart>

### Donut

<deckgo-pie-chart width={500} height={400} inner-radius={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"></deckgo-pie-chart>

### Animated pie

<div>
  <deckgo-pie-chart id="animatedPie" animation={true} width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"></deckgo-pie-chart>
  
  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedPie')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedPie')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

### Compare two graphs

<deckgo-line-chart width={500} height={400} y-axis-domain="extent" date-pattern="dd.MM.yyyy" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv"></deckgo-line-chart>

### Line and area

<deckgo-line-chart width={500} height={400} smooth={false} ticks={5} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart.csv" date-pattern="dd.MM.yyyy"></deckgo-line-chart>

### Compare two lines and smoothing effect

<div>
  <deckgo-line-chart width={500} height={400} area={false} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-no-dates.csv"
                    style={{'--deckgo-chart-fill-color-1': 'none', '--deckgo-chart-stroke-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'none', '--deckgo-chart-fill-opacity-2': '1', '--deckgo-chart-stroke-2': 'var(--ion-color-secondary)', '--deckgo-chart-stroke-width-2': '3px'}}></deckgo-line-chart>
</div>

### Compare two lines and display a grid

<div>
  <deckgo-line-chart width={500} height={400} smooth={false} area={false} grid={true} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-no-dates.csv"
                    style={{'--deckgo-chart-fill-color-1': 'none', '--deckgo-chart-stroke-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'none', '--deckgo-chart-fill-opacity-2': '1', '--deckgo-chart-stroke-2': 'var(--ion-color-secondary)', '--deckgo-chart-stroke-width-2': '3px'}}></deckgo-line-chart>
</div>

### Animated lines

<div>
  <deckgo-line-chart id="animatedLine" animation={true} y-axis-domain="extent" date-pattern="dd.MM.yyyy" width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-multiple.csv"></deckgo-line-chart>
  
  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedLine')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedLine')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

### Bar

<div>
  <deckgo-bar-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                    style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'}}></deckgo-bar-chart>
</div>
                    
### Compare multiple bars

<div>
  <deckgo-bar-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv" 
                    style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)'}}></deckgo-bar-chart>
</div>

### Animated bars

<div>
  <deckgo-bar-chart id="animatedBar" animation={true} width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare-with-titles.csv"
                      style={{'--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'}}></deckgo-bar-chart>
  
  <div>
    <ion-button fill="outline" size="small" onClick={() => this.prev('animatedBar')}><ion-label>Prev</ion-label></ion-button>
    <ion-button fill="outline" size="small" onClick={() => this.next('animatedBar')}><ion-label>Next</ion-label></ion-button>
  </div>
</div>

### Video

Have a look at this video where we demonstrate it!

<iframe width="560" height="315" src="https://www.youtube.com/embed/CaEwUXO7BYA" frameborder="0"></iframe>

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using DeckDeckGo charts from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] charts from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/charts@latest/dist/deckdeckgo-charts/deckdeckgo-charts.esm.js"></script>
```

### Install DeckDeckGo charts from NPM

Install [DeckDeckGo] charts in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/charts) using the following command:

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

The main idea behind the [DeckDeckGo] charts is to be able to drop a **csv** file, provide it to the component and let it do the job respectively draw the chart.

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

#### Properties

The `<deckgo-pie-chart/>` expose the following properties:

| Property            | Attribute            | Mandatory | Description                                                                                                                     | Type      | Default               |
| ------------------- | -------------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------- | --------- | --------------------- |
| `src`               | `src`                | X         | The path to the source file of the data                                                                                         | `string`  |                       |
| `width`             | `width`              | X         | The width of the chart                                                                                                          | `number`  |                       |
| `height`            | `height`             | X         | The height of the chart                                                                                                         | `number`  |                       |
| `innerRadius`       | `inner-radius`       |           | To plot a `donut` instead of a `pie`, provide an inner radius                                                                   | `number`  | `0`                   |
| `separator`         | `separator`          |           | The line separator use in your csv file                                                                                         | `string`  | `';'`                 |
| `animation`         | `animation`          |           | Display multiple graphs and animate the transition between these                                                                | `boolean` | `false`               |
| `animationDuration` | `animation-duration` |           | Duration of the transition between graphs                                                                                       | `numer`   | `1000` (aka 1 second) |
| `marginBottom`      | `margin-bottom`      |           | The margin bottom of the chart in pixel                                                                                         | `number`  | `64`                  |
| `marginLeft`        | `margin-left`        |           | The margin left of the chart in pixel                                                                                           | `number`  | `32`                  |
| `marginRight`       | `margin-right`       |           | The margin right of the chart in pixel                                                                                          | `number`  | `32`                  |
| `marginTop`         | `margin-top`         |           | The margin top of the chart in pixel                                                                                            | `number`  | `8`                   |
| `customLoader`      | `custom-loader`      |           | Set to `true` in case you would like to load (fetch) the data by yourself. Useful in case your data are protected with a token. | `boolean` | `false`               |

Note: if you would use `customLoader` the component will emit an event `customLoad` which the source of the data to load. You should subscribe to this event in order to process your custom load.

#### Styling

The `<deckgo-pie-chart/>` could be styled using the following CSS4 variables:

| CSS4 variable                   | Default               | Note                                                                                        |
| ------------------------------- | --------------------- | ------------------------------------------------------------------------------------------- |
| --deckgo-chart-text-color       | black                 | The color of the labels                                                                     |
| --deckgo-chart-text-display     |                       | The display property of the labels                                                          |
| --deckgo-chart-path-stroke      |                       | The stroke property of the pie, in case you would like to display a line between the slices |
| --deckgo-chart-text-font-size   |                       | The font size of the labels of the chart                                                    |
| --deckgo-chart-text-font-family | inherit               | The font family of the labels of the chart                                                  |
| --deckgo-chart-position         | relative              | The position of the chart host                                                              |
| --deckgo-chart-display          | block                 | The display property of the chart host                                                      |
| --deckgo-chart-margin           | 0                     | The margin of the chart host                                                                |
| --deckgo-chart-svg-overflow     |                       | The overflow property of the svg generated by the chart                                     |
| --deckgo-chart-svg-position     | absolute              | The position of the svg generated by the chart                                              |
| --deckgo-chart-svg-top          | 50%                   | The top position of the svg generated by the chart                                          |
| --deckgo-chart-svg-left         | 50%                   | The left position of the svg generated by the chart                                         |
| --deckgo-chart-svg-translate    | translate(-50%, -50%) | The translate property of the svg generated by the chart                                    |

Furthermore, as the Pie chart contains probably more than one slice, it will generate dynamically the following CSS4 variable for each slice of the pie where `X` is an index between `1` and the number of slices.

| CSS4 variable                     | Default | Note                                                    |
| --------------------------------- | ------- | ------------------------------------------------------- |
| --deckgo-chart-fill-color-index   |         | The fill color of the slice identified with index `X`   |
| --deckgo-chart-fill-opacity-index |         | The opacity of the slice identified with index `X`      |
| --deckgo-chart-stroke-index       |         | The stroke of the slice identified with index `X`       |
| --deckgo-chart-stroke-width-index |         | The stroke width of the slice identified with index `X` |

#### Methods

The `<deckgo-pie-chart/>` expose the following methods.

##### Draw

In case you would like to redraw your chart, for example on resize of the window:

```
draw(width?: number, height?: number) => Promise<void>
```

##### Next

If you are using `animation`, this method is used to display the next data respectively the next chart.

```
async next()
```

##### Previous

If you are using `animation`, this method is used to display the previous data respectively the previous chart.

```
async prev()
```

##### Post load data

If you "manually" load the data, call this method once the `text` content fetched.

```
postCustomLoad(content: string | undefined);
```

#### Examples

You could find other examples of pie charts in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html) of the project.

```
<deckgo-pie-chart width={500} height={400} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"></deckgo-pie-chart>
```

### Line usage

The `<deckgo-line-chart/>` Web Component draw a line chart.

#### CSV

The csv file should contains two or multiple columns.

The first column contains the values for the `x` axis. These should be provided as `date` or `number`.

The second and other columns contains the values for the `y` axis. These should be provided as `number`.

Use multiple columns in case you would like to compare multiple graphs.

##### Two columns

With dates as `x` axis:

```
01.01.2018;5
01.03.2018;10
01.06.2018;8
```

With numbers as `x` axis:

```
1;5
2;10
3;8
```

##### Three columns or more

The third columns or any others is optional, it could be use in case you would like to plot multiple charts on the same graph or animate a transition between these.

With dates as `x` axis:

```
01.01.2018;5;4
01.03.2018;10;3
01.06.2018;8;19
```

With numbers as `x` axis:

```
1;5;7
2;10;13
3;8;5
```

#### Properties

The `<deckgo-line-chart/>` expose the following properties:

| Property            | Attribute            | Mandatory | Description                                                                                                                                                                                 | Type      | Default               |
| ------------------- | -------------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------- | --------------------- |
| `src`               | `src`                | X         | The path to the source file of the data                                                                                                                                                     | `string`  |                       |
| `width`             | `width`              | X         | The width of the chart                                                                                                                                                                      | `number`  |                       |
| `height`            | `height`             | X         | The height of the chart                                                                                                                                                                     | `number`  |                       |
| `marginBottom`      | `margin-bottom`      |           | The margin bottom of the chart in pixel                                                                                                                                                     | `number`  | `32`                  |
| `marginLeft`        | `margin-left`        |           | The margin left of the chart in pixel                                                                                                                                                       | `number`  | `32`                  |
| `marginRight`       | `margin-right`       |           | The margin right of the chart in pixel                                                                                                                                                      | `number`  | `32`                  |
| `marginTop`         | `margin-top`         |           | The margin top of the chart in pixel                                                                                                                                                        | `number`  | `32`                  |
| `datePattern`       | `date-pattern`       |           | In case the `x` are made of dates, the pattern to be use to parse the dates. Have a look to [date-fns](https://date-fns.org/v2.0.0-alpha.26/docs/parse) for a list of the supported format. | `string`  | `'yyyy-MM-dd'`        |
| `yAxisDomain`       | `y-axis-domain`      |           | Should the `y` axis plot the values from `0` to `max` or be extended (use `extent`) to cover all values                                                                                     | `string`  | `max`                 |
| `smooth`            | `smooth`             |           | Draw a smooth line or a line with edges                                                                                                                                                     | `boolean` | `true`                |
| `area`              | `area`               |           | Draw the area or just a line                                                                                                                                                                | `boolean` | `true`                |
| `ticks`             | `ticks`              |           | Specify the ticks of the axis                                                                                                                                                               | `number`  | ``                    |
| `grid`              | `grid`               |           | Draw a grid behin the graph                                                                                                                                                                 | `boolean` | `false`               |
| `separator`         | `separator`          |           | The line separator use in your csv file                                                                                                                                                     | `string`  | `';'`                 |
| `animation`         | `animation`          |           | Display multiple graphs and animate the transition between these                                                                                                                            | `boolean` | `false`               |
| `animationDuration` | `animation-duration` |           | Duration of the transition between graphs                                                                                                                                                   | `numer`   | `1000` (aka 1 second) |
| `customLoader`      | `custom-loader`      |           | Set to `true` in case you would like to load (fetch) the data by yourself. Useful in case your data are protected with a token.                                                             | `boolean` | `false`               |

Note: if you would use `customLoader` the component will emit an event `customLoad` which the source of the data to load. You should subscribe to this event in order to process your custom load.

#### Styling

The `<deckgo-line-chart/>` could be styled using the following CSS4 variables:

| CSS4 variable                      | Default               | Note                                                        |
| ---------------------------------- | --------------------- | ----------------------------------------------------------- |
| --deckgo-chart-axis-color          | black                 | The color of the axis                                       |
| --deckgo-chart-text-color          | black                 | The color of the labels                                     |
| --deckgo-chart-text-display        |                       | The display property of the labels                          |
| --deckgo-chart-grid-stroke         | #989aa2               | The stroke of the grid                                      |
| --deckgo-chart-grid-stroke-opacity | 0.7                   | The stroke opacity of the grid                              |
| --deckgo-chart-stroke              |                       | A default stroke which could be applied to all graphs' line |
| --deckgo-chart-text-font-size      |                       | The font size of the labels of the chart                    |
| --deckgo-chart-text-font-family    | inherit               | The font family of the labels of the chart                  |
| --deckgo-chart-position            | relative              | The position of the chart host                              |
| --deckgo-chart-display             | block                 | The display property of the chart host                      |
| --deckgo-chart-margin              | 32px                  | The margin of the chart host                                |
| --deckgo-chart-svg-overflow        |                       | The overflow property of the svg generated by the chart     |
| --deckgo-chart-svg-position        | absolute              | The position of the svg generated by the chart              |
| --deckgo-chart-svg-top             | 50%                   | The top position of the svg generated by the chart          |
| --deckgo-chart-svg-left            | 50%                   | The left position of the svg generated by the chart         |
| --deckgo-chart-svg-translate       | translate(-50%, -50%) | The translate property of the svg generated by the chart    |

Furthermore, for each generated charts, the following CSS4 variables could be applied:

| CSS4 variable                     | Default | Note                                      |
| --------------------------------- | ------- | ----------------------------------------- |
| --deckgo-chart-fill-color-index   | #3880ff | The fill color of the area of the chart   |
| --deckgo-chart-fill-opacity-index |         | The opacity of the area of the chart      |
| --deckgo-chart-stroke-index       |         | The stroke of the area of the chart       |
| --deckgo-chart-stroke-width-index |         | The stroke width of the area of the chart |

Note: Replace `-index` with the index of the chart, for example: `--deckgo-chart-fill-color-1: red;`

#### Methods

The `<deckgo-line-chart/>` expose the following methods.

##### Draw

In case you would like to redraw your chart, for example on resize of the window:

```
draw(width?: number, height?: number) => Promise<void>
```

##### Next

If you are using `animation`, this method is used to display the next data respectively the next chart.

```
async next()
```

##### Previous

If you are using `animation`, this method is used to display the previous data respectively the previous chart.

```
async prev()

```

##### Post load data

If you "manually" load the data, call this method once the `text` content fetched.

```
postCustomLoad(content: string | undefined);
```

#### Examples

You could find other examples of line charts in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html) of the project.

```
<deckgo-line-chart width={500} height={400} date-pattern="dd.MM.yyyy" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart.csv"></deckgo-line-chart>
```

### Bar usage

The `<deckgo-bar-chart/>` Web Component draw a Bar chart.

#### CSV

The csv file should contains at least two columns. The first column should contains the labels. The other columns should contains values.

Use multiple columns in case you would like to compare multiple graphs.

The values could be provided as `number` or `percent`.

##### Multiple columns

The first column should contains the labels or groups used for the X axis. The other columns are the values use for the Y axis.

```
Group A;5;0;10
Group B;10;6;12
Group C;8;14;8
Group D;14;8;16
Group E;18;4;5
```

If you want to provide title for the bars, add a first row in your data with an empty value for the first column.

```
;Salami;Pastrami;Prosciutto
Group A;5;0;10
Group B;10;6;12
Group C;8;14;8
Group D;14;8;16
Group E;18;4;5
```

#### Properties

The `<deckgo-bar-chart/>` expose the following properties:

| Property            | Attribute            | Mandatory | Description                                                                                                                     | Type                     | Default               |
| ------------------- | -------------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------- | ------------------------ | --------------------- |
| `src`               | `src`                | X         | The path to the source file of the data                                                                                         | `string`                 |                       |
| `width`             | `width`              | X         | The width of the chart                                                                                                          | `number`                 |                       |
| `height`            | `height`             | X         | The height of the chart                                                                                                         | `number`                 |                       |
| `marginBottom`      | `margin-bottom`      |           | The margin bottom of the chart in pixel                                                                                         | `number`                 | `32`                  |
| `marginLeft`        | `margin-left`        |           | The margin left of the chart in pixel                                                                                           | `number`                 | `32`                  |
| `marginRight`       | `margin-right`       |           | The margin right of the chart in pixel                                                                                          | `number`                 | `32`                  |
| `marginTop`         | `margin-top`         |           | The margin top of the chart in pixel                                                                                            | `number`                 | `32`                  |
| `separator`         | `separator`          |           | The line separator use in your csv file                                                                                         | `string`                 | `';'`                 |
| `animation`         | `animation`          |           | Display multiple graphs and animate the transition between these                                                                | `boolean`                | `false`               |
| `animationDuration` | `animation-duration` |           | Duration of the transition between graphs                                                                                       | `numer`                  | `1000` (aka 1 second) |
| `data`              | `data`               |           | Instead of a source file it is also possible to provide the data of the chart as an object                                      | `DeckdeckgoBarChartData` |                       |
| `yAxis`             | `y-axis`             |           | If `false`, no axis y will be draw.                                                                                             | `boolean`                |                       |
| `customLoader`      | `custom-loader`      |           | Set to `true` in case you would like to load (fetch) the data by yourself. Useful in case your data are protected with a token. | `boolean`                | `false`               |
| `yAxisMin`          | `y-axis-min`         |           | Set a minimal value for the y Axis. Useful in case the series of data could contains only zeros.                                |

Note: if you would use `customLoader` the component will emit an event `customLoad` which the source of the data to load. You should subscribe to this event in order to process your custom load.

#### Styling

The `<deckgo-bar-chart/>` could be styled using the following CSS4 variables:

| CSS4 variable             | Default | Note                  |
| ------------------------- | ------- | --------------------- |
| --deckgo-chart-axis-color | black   | The color of the axis |

Furthermore, as the Bar chart could draw dynamically multiple bars, it will generate dynamically the following CSS4 variable for each series of data where `X` is an index between `1` and the number of bars.

| CSS4 variable                     | Default               | Note                                                      |
| --------------------------------- | --------------------- | --------------------------------------------------------- |
| --deckgo-chart-text-color         |                       | The color of the labels                                   |
| --deckgo-chart-text-display       |                       | The display property of the labels                        |
| --deckgo-chart-fill-color-index   |                       | The fill color of the bar chart identified with index `X` |
| --deckgo-chart-fill-opacity-index |                       | The opacity of the bar chart identified with index `X`    |
| --deckgo-chart-stroke-index       |                       | The stroke of the bar chart identified with index `X`     |
| --deckgo-chart-stroke-width-index |                       | The stroke width of the chart identified with index `X`   |
| --deckgo-chart-text-font-size     |                       | The font size of the labels of the chart                  |
| --deckgo-chart-text-font-family   | inherit               | The font family of the labels of the chart                |
| --deckgo-chart-position           | relative              | The position of the chart host                            |
| --deckgo-chart-display            | block                 | The display property of the chart host                    |
| --deckgo-chart-margin             | 0                     | The margin of the chart host                              |
| --deckgo-chart-svg-overflow       |                       | The overflow property of the svg generated by the chart   |
| --deckgo-chart-svg-position       | absolute              | The position of the svg generated by the chart            |
| --deckgo-chart-svg-top            | 50%                   | The top position of the svg generated by the chart        |
| --deckgo-chart-svg-left           | 50%                   | The left position of the svg generated by the chart       |
| --deckgo-chart-svg-translate      | translate(-50%, -50%) | The translate property of the svg generated by the chart  |

#### Methods

The `<deckgo-bar-chart/>` expose the following methods:

##### Draw

In case you would like to redraw your chart, for example on resize of the window:

```
draw(width?: number, height?: number) => Promise<void>
```

##### Next

If you are using `animation`, this method is used to display the next data respectively the next chart.

```
async next()
```

##### Previous

If you are using `animation`, this method is used to display the previous data respectively the previous chart.

```
async prev()
```

##### Post load data

If you "manually" load the data, call this method once the `text` content fetched.

```
postCustomLoad(content: string | undefined);
```

##### Updating current bar

This is the method we are using to refresh the current bar chart when an audience is participating to live vote. It will not redraw the axis but it will draw and animate the bars.

```
async updateCurrentBar(values: DeckdeckgoBarChartDataValue[])
```

#### Examples

You could find other examples of bar charts in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/charts/src/index.html) of the project.

```
<deckgo-bar-chart width={500}
                  height={400}
                   src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv"
                   style="--deckgo-chart-fill-color-1: #3880ff;">
</deckgo-bar-chart>
```

[deckdeckgo]: https://deckdeckgo.com
