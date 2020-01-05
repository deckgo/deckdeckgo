# deckgo-slide-author



<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description | Type                            | Default     |
| ------------------ | ------------------- | ----------- | ------------------------------- | ----------- |
| `customActions`    | `custom-actions`    |             | `boolean`                       | `false`     |
| `customBackground` | `custom-background` |             | `boolean`                       | `false`     |
| `imgAlt`           | `img-alt`           |             | `string`                        | `undefined` |
| `imgMode`          | `img-mode`          |             | `"circle" \| "cover" \| "none"` | `'cover'`   |
| `imgSrc`           | `img-src`           |             | `string`                        | `undefined` |


## Events

| Event          | Description | Type                |
| -------------- | ----------- | ------------------- |
| `slideDidLoad` |             | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(_enter: boolean, _reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



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
