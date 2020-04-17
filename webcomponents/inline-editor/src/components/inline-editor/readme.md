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

- [deckgo-ie-triangle](../components/triangle)
- [deckgo-ie-link-actions](../actions/link-actions)
- [deckgo-ie-color-actions](../actions/color-actions)
- [deckgo-ie-image-actions](../actions/image-actions)
- [deckgo-ie-align-actions](../actions/align-actions)
- [deckgo-ie-list-actions](../actions/list-actions)
- [deckgo-ie-style-actions](../actions/style-actions)
- [deckgo-ie-action-button](../components/action-button)
- [deckgo-ie-action-image](../components/action-image)
- [deckgo-ie-separator](../components/separator)

### Graph

```mermaid
graph TD;
  deckgo-inline-editor --> deckgo-ie-triangle
  deckgo-inline-editor --> deckgo-ie-link-actions
  deckgo-inline-editor --> deckgo-ie-color-actions
  deckgo-inline-editor --> deckgo-ie-image-actions
  deckgo-inline-editor --> deckgo-ie-align-actions
  deckgo-inline-editor --> deckgo-ie-list-actions
  deckgo-inline-editor --> deckgo-ie-style-actions
  deckgo-inline-editor --> deckgo-ie-action-button
  deckgo-inline-editor --> deckgo-ie-action-image
  deckgo-inline-editor --> deckgo-ie-separator
  deckgo-ie-color-actions --> deckgo-color
  deckgo-ie-image-actions --> deckgo-ie-action-button
  deckgo-ie-image-actions --> deckgo-ie-action-image
  deckgo-ie-image-actions --> deckgo-ie-separator
  deckgo-ie-align-actions --> deckgo-ie-action-button
  deckgo-ie-align-actions --> deckgo-ie-action-image
  deckgo-ie-list-actions --> deckgo-ie-action-button
  deckgo-ie-list-actions --> deckgo-ie-action-image
  deckgo-ie-style-actions --> deckgo-ie-action-button
  style deckgo-inline-editor fill:#f9f,stroke:#333,stroke-width:4px
```

---

_Built with [StencilJS](https://stenciljs.com/)_
