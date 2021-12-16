# deckgo-demo



<!-- Auto Generated Below -->


## Properties

| Property     | Attribute     | Description                                                                                                  | Type      | Default     |
| ------------ | ------------- | ------------------------------------------------------------------------------------------------------------ | --------- | ----------- |
| `frameTitle` | `frame-title` | A title for the frame, could be use for accessibility reason                                                 | `string`  | `undefined` |
| `instant`    | `instant`     | In case you would like to load the frame as soon as the component is loaded                                  | `boolean` | `false`     |
| `mode`       | `mode`        | The type of device frame. md for Android, ios for iPhone                                                     | `string`  | `'md'`      |
| `src`        | `src`         | The source Url of your application or website. This will be used as src attribute of the encapsulated iframe | `string`  | `undefined` |


## Methods

### `lazyLoadContent() => Promise<void>`

Lazy load the iframe

#### Returns

Type: `Promise<void>`



### `updateIFrame() => Promise<void>`

Refresh iframe size and reload content

#### Returns

Type: `Promise<void>`




----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
