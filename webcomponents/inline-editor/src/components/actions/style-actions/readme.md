# deckgo-ie-style-actions



<!-- Auto Generated Below -->


## Properties

| Property        | Attribute        | Description | Type        | Default     |
| --------------- | ---------------- | ----------- | ----------- | ----------- |
| `bold`          | `bold`           |             | `boolean`   | `undefined` |
| `disabledTitle` | `disabled-title` |             | `boolean`   | `false`     |
| `italic`        | `italic`         |             | `boolean`   | `undefined` |
| `mobile`        | `mobile`         |             | `boolean`   | `undefined` |
| `selection`     | --               |             | `Selection` | `undefined` |
| `strikethrough` | `strikethrough`  |             | `boolean`   | `undefined` |
| `underline`     | `underline`      |             | `boolean`   | `undefined` |


## Events

| Event         | Description | Type                             |
| ------------- | ----------- | -------------------------------- |
| `execCommand` |             | `CustomEvent<ExecCommandAction>` |


## Dependencies

### Used by

 - [deckgo-inline-editor](../../inline-editor)

### Depends on

- [deckgo-ie-action-button](../../components/action-button)

### Graph
```mermaid
graph TD;
  deckgo-ie-style-actions --> deckgo-ie-action-button
  deckgo-inline-editor --> deckgo-ie-style-actions
  style deckgo-ie-style-actions fill:#f9f,stroke:#333,stroke-width:4px
```

----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
