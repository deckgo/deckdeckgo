# deckdeckgo-code



<!-- Auto Generated Below -->


## Properties

| Property         | Attribute         | Description | Type      | Default               |
| ---------------- | ----------------- | ----------- | --------- | --------------------- |
| `anchor`         | `anchor`          |             | `string`  | `'// DeckDeckGo'`     |
| `anchorZoom`     | `anchor-zoom`     |             | `string`  | `'// DeckDeckGoZoom'` |
| `editable`       | `editable`        |             | `boolean` | `false`               |
| `hideAnchor`     | `hide-anchor`     |             | `boolean` | `true`                |
| `highlightLines` | `highlight-lines` |             | `string`  | `undefined`           |
| `language`       | `language`        |             | `string`  | `'javascript'`        |
| `src`            | `src`             |             | `string`  | `undefined`           |


## Events

| Event                 | Description | Type                       |
| --------------------- | ----------- | -------------------------- |
| `codeDidChange`       |             | `CustomEvent<HTMLElement>` |
| `prismLanguageLoaded` |             | `CustomEvent<string>`      |


## Methods

### `findNextAnchor(enter: boolean) => Promise<DeckdeckgoHighlightCodeAnchor>`



#### Returns

Type: `Promise<DeckdeckgoHighlightCodeAnchor>`



### `load() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `zoomCode(zoom: boolean) => Promise<void>`



#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
