# deckdeckgo-code



<!-- Auto Generated Below -->


## Properties

| Property         | Attribute         | Description | Type      | Default               |
| ---------------- | ----------------- | ----------- | --------- | --------------------- |
| `anchor`         | `anchor`          |             | `string`  | `'// DeckDeckGo'`     |
| `anchorZoom`     | `anchor-zoom`     |             | `string`  | `'// DeckDeckGoZoom'` |
| `hideAnchor`     | `hide-anchor`     |             | `boolean` | `true`                |
| `highlightLines` | `highlight-lines` |             | `string`  | `undefined`           |
| `language`       | `language`        |             | `string`  | `'javascript'`        |
| `src`            | `src`             |             | `string`  | `undefined`           |


## Events

| Event                 | Description | Type                  |
| --------------------- | ----------- | --------------------- |
| `prismLanguageLoaded` |             | `CustomEvent<string>` |


## Methods

### `findNextAnchor(enter: boolean) => Promise<DeckdeckgoHighlightCodeAnchor>`



#### Parameters

| Name    | Type      | Description |
| ------- | --------- | ----------- |
| `enter` | `boolean` |             |

#### Returns

Type: `Promise<DeckdeckgoHighlightCodeAnchor>`



### `load() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `zoomCode(zoom: boolean) => Promise<void>`



#### Parameters

| Name   | Type      | Description |
| ------ | --------- | ----------- |
| `zoom` | `boolean` |             |

#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
