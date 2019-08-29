# deckgo-slide-code



<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description | Type      | Default               |
| ------------------ | ------------------- | ----------- | --------- | --------------------- |
| `anchor`           | `anchor`            |             | `string`  | `'// DeckDeckGo'`     |
| `anchorZoom`       | `anchor-zoom`       |             | `string`  | `'// DeckDeckGoZoom'` |
| `customActions`    | `custom-actions`    |             | `boolean` | `false`               |
| `customBackground` | `custom-background` |             | `boolean` | `false`               |
| `hideAnchor`       | `hide-anchor`       |             | `boolean` | `true`                |
| `language`         | `language`          |             | `string`  | `'javascript'`        |
| `src`              | `src`               |             | `string`  | `undefined`           |


## Events

| Event          | Description | Type                   |
| -------------- | ----------- | ---------------------- |
| `scrolling`    |             | `CustomEvent<boolean>` |
| `slideDidLoad` |             | `CustomEvent<void>`    |


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
