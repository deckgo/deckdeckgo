# deckgo-slide-chart



<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description | Type       | Default                        |
| ------------------- | -------------------- | ----------- | ---------- | ------------------------------ |
| `animation`         | `animation`          |             | `boolean`  | `false`                        |
| `animationDuration` | `animation-duration` |             | `number`   | `1000`                         |
| `area`              | `area`               |             | `boolean`  | `true`                         |
| `customActions`     | `custom-actions`     |             | `boolean`  | `false`                        |
| `customBackground`  | `custom-background`  |             | `boolean`  | `false`                        |
| `datePattern`       | `date-pattern`       |             | `string`   | `'yyyy-MM-dd'`                 |
| `grid`              | `grid`               |             | `boolean`  | `false`                        |
| `height`            | `height`             |             | `number`   | `undefined`                    |
| `innerRadius`       | `inner-radius`       |             | `number`   | `0`                            |
| `marginBottom`      | `margin-bottom`      |             | `number`   | `32`                           |
| `marginLeft`        | `margin-left`        |             | `number`   | `32`                           |
| `marginRight`       | `margin-right`       |             | `number`   | `32`                           |
| `marginTop`         | `margin-top`         |             | `number`   | `32`                           |
| `range`             | --                   |             | `string[]` | `undefined`                    |
| `separator`         | `separator`          |             | `string`   | `';'`                          |
| `smooth`            | `smooth`             |             | `boolean`  | `true`                         |
| `src`               | `src`                |             | `string`   | `undefined`                    |
| `ticks`             | `ticks`              |             | `number`   | `undefined`                    |
| `type`              | `type`               |             | `string`   | `DeckdeckgoSlideChartType.PIE` |
| `width`             | `width`              |             | `number`   | `undefined`                    |
| `yAxisDomain`       | `y-axis-domain`      |             | `string`   | `'max'`                        |


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



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
