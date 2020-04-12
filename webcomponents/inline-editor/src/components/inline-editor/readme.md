# deckgo-inline-editor

<!-- Auto Generated Below -->

## Properties

| Property              | Attribute                | Description | Type                  | Default                   |
| --------------------- | ------------------------ | ----------- | --------------------- | ------------------------- |
| `attachTo`            | --                       |             | `HTMLElement`         | `undefined`               |
| `containers`          | `containers`             |             | `string`              | `'h1,h2,h3,h4,h5,h6,div'` |
| `customActions`       | `custom-actions`         |             | `string`              | `undefined`               |
| `imgAnchor`           | `img-anchor`             |             | `string`              | `'img'`                   |
| `imgEditable`         | `img-editable`           |             | `boolean`             | `false`                   |
| `imgPropertyCssFloat` | `img-property-css-float` |             | `string`              | `'float'`                 |
| `imgPropertyWidth`    | `img-property-width`     |             | `string`              | `'width'`                 |
| `list`                | `list`                   |             | `boolean`             | `true`                    |
| `mobile`              | `mobile`                 |             | `boolean`             | `false`                   |
| `palette`             | --                       |             | `DeckdeckgoPalette[]` | `DEFAULT_PALETTE`         |
| `stickyDesktop`       | `sticky-desktop`         |             | `boolean`             | `false`                   |
| `stickyMobile`        | `sticky-mobile`          |             | `boolean`             | `false`                   |

## Events

| Event                    | Description | Type                        |
| ------------------------ | ----------- | --------------------------- |
| `customAction`           |             | `CustomEvent<InlineAction>` |
| `imgDidChange`           |             | `CustomEvent<HTMLElement>`  |
| `linkCreated`            |             | `CustomEvent<HTMLElement>`  |
| `stickyToolbarActivated` |             | `CustomEvent<boolean>`      |

## Methods

### `reset(clearSelection: boolean, blurActiveElement?: boolean) => Promise<void>`

#### Returns

Type: `Promise<void>`

## Dependencies

### Depends on

- [deckgo-ie-action-button](../action-button)
- [deckgo-ie-action-image](../action-image)
- [deckgo-ie-link-actions](../link-actions)
- deckgo-color
- [deckgo-ie-image-actions](../image-actions)
- [deckgo-ie-separator](../separator)

### Graph

```mermaid
graph TD;
  deckgo-inline-editor --> deckgo-ie-action-button
  deckgo-inline-editor --> deckgo-ie-action-image
  deckgo-inline-editor --> deckgo-ie-link-actions
  deckgo-inline-editor --> deckgo-color
  deckgo-inline-editor --> deckgo-ie-image-actions
  deckgo-inline-editor --> deckgo-ie-separator
  deckgo-ie-image-actions --> deckgo-ie-action-button
  deckgo-ie-image-actions --> deckgo-ie-action-image
  deckgo-ie-image-actions --> deckgo-ie-separator
  style deckgo-inline-editor fill:#f9f,stroke:#333,stroke-width:4px
```

---

_Built with [StencilJS](https://stenciljs.com/)_
