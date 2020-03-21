# deckdeckgo-code

<!-- Auto Generated Below -->

## Properties

| Property         | Attribute         | Description | Type                             | Default               |
| ---------------- | ----------------- | ----------- | -------------------------------- | --------------------- |
| `anchor`         | `anchor`          |             | `string`                         | `'// DeckDeckGo'`     |
| `anchorZoom`     | `anchor-zoom`     |             | `string`                         | `'// DeckDeckGoZoom'` |
| `editable`       | `editable`        |             | `boolean`                        | `false`               |
| `hideAnchor`     | `hide-anchor`     |             | `boolean`                        | `true`                |
| `highlightLines` | `highlight-lines` |             | `string`                         | `undefined`           |
| `language`       | `language`        |             | `string`                         | `'javascript'`        |
| `lineNumbers`    | `line-numbers`    |             | `boolean`                        | `false`               |
| `src`            | `src`             |             | `string`                         | `undefined`           |
| `terminal`       | `terminal`        |             | `"carbon" \| "none" \| "ubuntu"` | `'carbon'`            |

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

---

_Built with [StencilJS](https://stenciljs.com/)_
