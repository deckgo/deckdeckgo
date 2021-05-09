# deckgo-ie-list-actions



<!-- Auto Generated Below -->


## Properties

| Property        | Attribute        | Description | Type                                           | Default     |
| --------------- | ---------------- | ----------- | ---------------------------------------------- | ----------- |
| `contentList`   | `content-list`   |             | `ContentList.ORDERED \| ContentList.UNORDERED` | `undefined` |
| `disabledTitle` | `disabled-title` |             | `boolean`                                      | `false`     |
| `mobile`        | `mobile`         |             | `boolean`                                      | `undefined` |
| `sticky`        | `sticky`         |             | `boolean`                                      | `undefined` |


## Events

| Event         | Description | Type                             |
| ------------- | ----------- | -------------------------------- |
| `execCommand` |             | `CustomEvent<ExecCommandAction>` |


## Dependencies

### Used by

 - [deckgo-inline-editor](../../inline-editor)

### Depends on

- [deckgo-ie-action-button](../../components/action-button)
- [deckgo-ie-action-image](../../components/action-image)

### Graph
```mermaid
graph TD;
  deckgo-ie-list-actions --> deckgo-ie-action-button
  deckgo-ie-list-actions --> deckgo-ie-action-image
  deckgo-inline-editor --> deckgo-ie-list-actions
  style deckgo-ie-list-actions fill:#f9f,stroke:#333,stroke-width:4px
```

----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
