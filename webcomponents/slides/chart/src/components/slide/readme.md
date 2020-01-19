# deckgo-slide-chart



<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description | Type       | Default                        |
| ------------------- | -------------------- | ----------- | ---------- | ------------------------------ |
| `animation`         | `animation`          |             | `boolean`  | `false`                        |
| `animationDuration` | `animation-duration` |             | `number`   | `1000`                         |
| `area`              | `area`               |             | `string`   | `undefined`                    |
| `customActions`     | `custom-actions`     |             | `boolean`  | `false`                        |
| `customBackground`  | `custom-background`  |             | `boolean`  | `false`                        |
| `customLoader`      | `custom-loader`      |             | `boolean`  | `false`                        |
| `datePattern`       | `date-pattern`       |             | `string`   | `undefined`                    |
| `grid`              | `grid`               |             | `string`   | `undefined`                    |
| `height`            | `height`             |             | `number`   | `undefined`                    |
| `innerRadius`       | `inner-radius`       |             | `number`   | `undefined`                    |
| `marginBottom`      | `margin-bottom`      |             | `number`   | `64`                           |
| `marginLeft`        | `margin-left`        |             | `number`   | `32`                           |
| `marginRight`       | `margin-right`       |             | `number`   | `32`                           |
| `marginTop`         | `margin-top`         |             | `number`   | `8`                            |
| `range`             | --                   |             | `string[]` | `undefined`                    |
| `separator`         | `separator`          |             | `string`   | `undefined`                    |
| `smooth`            | `smooth`             |             | `string`   | `undefined`                    |
| `src`               | `src`                |             | `string`   | `undefined`                    |
| `ticks`             | `ticks`              |             | `number`   | `undefined`                    |
| `type`              | `type`               |             | `string`   | `DeckdeckgoSlideChartType.PIE` |
| `width`             | `width`              |             | `number`   | `undefined`                    |
| `yAxisDomain`       | `y-axis-domain`      |             | `string`   | `undefined`                    |


## Events

| Event          | Description | Type                |
| -------------- | ----------- | ------------------- |
| `slideDidLoad` |             | `CustomEvent<void>` |


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



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `postCustomLoad(content: string) => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
