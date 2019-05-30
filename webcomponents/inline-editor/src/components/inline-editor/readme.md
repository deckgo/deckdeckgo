# deckgo-inline-editor



<!-- Auto Generated Below -->


## Properties

| Property              | Attribute                | Description | Type          | Default                   |
| --------------------- | ------------------------ | ----------- | ------------- | ------------------------- |
| `attachTo`            | --                       |             | `HTMLElement` | `undefined`               |
| `containers`          | `containers`             |             | `string`      | `'h1,h2,h3,h4,h5,h6,div'` |
| `imgAnchor`           | `img-anchor`             |             | `string`      | `'img'`                   |
| `imgPropertyCssFloat` | `img-property-css-float` |             | `string`      | `'cssFloat'`              |
| `imgPropertyWidth`    | `img-property-width`     |             | `string`      | `'width'`                 |
| `mobile`              | `mobile`                 |             | `boolean`     | `false`                   |
| `stickyDesktop`       | `sticky-desktop`         |             | `boolean`     | `false`                   |
| `stickyMobile`        | `sticky-mobile`          |             | `boolean`     | `false`                   |


## Events

| Event                    | Description | Type                       |
| ------------------------ | ----------- | -------------------------- |
| `imgDidChange`           |             | `CustomEvent<HTMLElement>` |
| `stickyToolbarActivated` |             | `CustomEvent<boolean>`     |


## Methods

### `reset(clearSelection: boolean, blurActiveElement?: boolean) => Promise<void>`



#### Parameters

| Name                | Type      | Description |
| ------------------- | --------- | ----------- |
| `clearSelection`    | `boolean` |             |
| `blurActiveElement` | `boolean` |             |

#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
