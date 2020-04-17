# deckgo-ie-align-actions

<!-- Auto Generated Below -->

## Properties

| Property       | Attribute       | Description | Type                                                             | Default     |
| -------------- | --------------- | ----------- | ---------------------------------------------------------------- | ----------- |
| `contentAlign` | `content-align` |             | `ContentAlign.CENTER \| ContentAlign.LEFT \| ContentAlign.RIGHT` | `undefined` |
| `mobile`       | `mobile`        |             | `boolean`                                                        | `undefined` |
| `selection`    | --              |             | `Selection`                                                      | `undefined` |

## Events

| Event       | Description | Type               |
| ----------- | ----------- | ------------------ |
| `initStyle` |             | `CustomEvent<any>` |

## Dependencies

### Used by

- [deckgo-inline-editor](../../inline-editor)

### Depends on

- [deckgo-ie-action-button](../../components/action-button)
- [deckgo-ie-action-image](../../components/action-image)

### Graph

```mermaid
graph TD;
  deckgo-ie-align-actions --> deckgo-ie-action-button
  deckgo-ie-align-actions --> deckgo-ie-action-image
  deckgo-inline-editor --> deckgo-ie-align-actions
  style deckgo-ie-align-actions fill:#f9f,stroke:#333,stroke-width:4px
```

---

_Built with [StencilJS](https://stenciljs.com/)_
