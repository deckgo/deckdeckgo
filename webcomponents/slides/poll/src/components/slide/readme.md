# deckgo-slide-qrcode



<!-- Auto Generated Below -->


## Properties

| Property            | Attribute             | Description | Type      | Default                         |
| ------------------- | --------------------- | ----------- | --------- | ------------------------------- |
| `connectPollSocket` | `connect-poll-socket` |             | `boolean` | `true`                          |
| `customActions`     | `custom-actions`      |             | `boolean` | `false`                         |
| `customBackground`  | `custom-background`   |             | `boolean` | `false`                         |
| `pollKey`           | `poll-key`            |             | `string`  | `undefined`                     |
| `pollLink`          | `poll-link`           |             | `string`  | `'https://deckdeckgo.com/poll'` |
| `socketPath`        | `socket-path`         |             | `string`  | `'/poll'`                       |
| `socketUrl`         | `socket-url`          |             | `string`  | `'https://api.deckdeckgo.com'`  |


## Events

| Event          | Description | Type                |
| -------------- | ----------- | ------------------- |
| `pollUpdated`  |             | `CustomEvent<void>` |
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



### `isAnswered() => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `update() => Promise<void>`



#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
