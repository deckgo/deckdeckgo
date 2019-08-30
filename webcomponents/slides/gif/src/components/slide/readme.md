# deckgo-slide-gif



<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description | Type      | Default     |
| ------------------ | ------------------- | ----------- | --------- | ----------- |
| `alt`              | `alt`               |             | `string`  | `undefined` |
| `customActions`    | `custom-actions`    |             | `boolean` | `false`     |
| `customBackground` | `custom-background` |             | `boolean` | `false`     |
| `fullscreen`       | `fullscreen`        |             | `boolean` | `true`      |
| `src`              | `src`               |             | `string`  | `undefined` |


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




## Dependencies

### Depends on

- [deckgo-gif](../gif)

### Graph
```mermaid
graph TD;
  deckgo-slide-gif --> deckgo-gif
  style deckgo-slide-gif fill:#f9f,stroke:#333,stroke-width:4px
```

----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
