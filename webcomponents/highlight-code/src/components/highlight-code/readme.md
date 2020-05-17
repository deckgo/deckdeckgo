# deckdeckgo-code

<!-- Auto Generated Below -->

## Properties

| Property         | Attribute         | Description | Type                                                                                                                                                                                                                                                                                                                                                                                                                                 | Default               |
| ---------------- | ----------------- | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------- |
| `anchor`         | `anchor`          |             | `string`                                                                                                                                                                                                                                                                                                                                                                                                                             | `'// DeckDeckGo'`     |
| `anchorZoom`     | `anchor-zoom`     |             | `string`                                                                                                                                                                                                                                                                                                                                                                                                                             | `'// DeckDeckGoZoom'` |
| `editable`       | `editable`        |             | `boolean`                                                                                                                                                                                                                                                                                                                                                                                                                            | `false`               |
| `hideAnchor`     | `hide-anchor`     |             | `boolean`                                                                                                                                                                                                                                                                                                                                                                                                                            | `true`                |
| `highlightLines` | `highlight-lines` |             | `string`                                                                                                                                                                                                                                                                                                                                                                                                                             | `undefined`           |
| `language`       | `language`        |             | `string`                                                                                                                                                                                                                                                                                                                                                                                                                             | `'javascript'`        |
| `lineNumbers`    | `line-numbers`    |             | `boolean`                                                                                                                                                                                                                                                                                                                                                                                                                            | `false`               |
| `src`            | `src`             |             | `string`                                                                                                                                                                                                                                                                                                                                                                                                                             | `undefined`           |
| `terminal`       | `terminal`        |             | `"carbon" \| "none" \| "ubuntu"`                                                                                                                                                                                                                                                                                                                                                                                                     | `'carbon'`            |
| `theme`          | `theme`           |             | `"3024-night" \| "a11y-dark" \| "blackboard" \| "base16-dark" \| "base16-light" \| "cobalt" \| "dracula" \| "duotone" \| "hopscotch" \| "lucario" \| "material" \| "monokai" \| "night-owl" \| "nord" \| "oceanic-next" \| "one-light" \| "one-dark" \| "panda" \| "paraiso" \| "seti" \| "shades-of-purple" \| "solarized-dark" \| "solarized-light" \| "synthwave" \| "twilight" \| "verminal" \| "vscode" \| "yeti" \| "zenburn"` | `'zenburn'`           |

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
