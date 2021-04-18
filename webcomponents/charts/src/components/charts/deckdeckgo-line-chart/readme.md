# deckgo-line-chart



<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description | Type      | Default                             |
| ------------------- | -------------------- | ----------- | --------- | ----------------------------------- |
| `animation`         | `animation`          |             | `boolean` | `false`                             |
| `animationDuration` | `animation-duration` |             | `number`  | `1000`                              |
| `area`              | `area`               |             | `boolean` | `true`                              |
| `customLoader`      | `custom-loader`      |             | `boolean` | `false`                             |
| `datePattern`       | `date-pattern`       |             | `string`  | `'yyyy-MM-dd'`                      |
| `grid`              | `grid`               |             | `boolean` | `false`                             |
| `height`            | `height`             |             | `number`  | `undefined`                         |
| `marginBottom`      | `margin-bottom`      |             | `number`  | `64`                                |
| `marginLeft`        | `margin-left`        |             | `number`  | `32`                                |
| `marginRight`       | `margin-right`       |             | `number`  | `32`                                |
| `marginTop`         | `margin-top`         |             | `number`  | `32`                                |
| `separator`         | `separator`          |             | `string`  | `';'`                               |
| `smooth`            | `smooth`             |             | `boolean` | `true`                              |
| `src`               | `src`                |             | `string`  | `undefined`                         |
| `ticks`             | `ticks`              |             | `number`  | `undefined`                         |
| `width`             | `width`              |             | `number`  | `undefined`                         |
| `yAxisDomain`       | `y-axis-domain`      |             | `string`  | `DeckdeckgoLineChartAxisDomain.MAX` |


## Events

| Event             | Description | Type                  |
| ----------------- | ----------- | --------------------- |
| `chartCustomLoad` |             | `CustomEvent<string>` |


## Methods

### `draw(width?: number, height?: number) => Promise<void>`



#### Returns

Type: `Promise<void>`



### `isBeginning() => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `isEnd() => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `next() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `postCustomLoad(content: string | undefined) => Promise<void>`



#### Returns

Type: `Promise<void>`



### `prev() => Promise<void>`



#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
