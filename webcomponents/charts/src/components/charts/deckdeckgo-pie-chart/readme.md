# deckgo-pie-chart



<!-- Auto Generated Below -->


## Properties

| Property            | Attribute            | Description | Type      | Default     |
| ------------------- | -------------------- | ----------- | --------- | ----------- |
| `animation`         | `animation`          |             | `boolean` | `false`     |
| `animationDuration` | `animation-duration` |             | `number`  | `1000`      |
| `customLoader`      | `custom-loader`      |             | `boolean` | `false`     |
| `height`            | `height`             |             | `number`  | `undefined` |
| `innerRadius`       | `inner-radius`       |             | `number`  | `0`         |
| `marginBottom`      | `margin-bottom`      |             | `number`  | `64`        |
| `marginLeft`        | `margin-left`        |             | `number`  | `32`        |
| `marginRight`       | `margin-right`       |             | `number`  | `32`        |
| `marginTop`         | `margin-top`         |             | `number`  | `8`         |
| `separator`         | `separator`          |             | `string`  | `';'`       |
| `src`               | `src`                |             | `string`  | `undefined` |
| `width`             | `width`              |             | `number`  | `undefined` |


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
