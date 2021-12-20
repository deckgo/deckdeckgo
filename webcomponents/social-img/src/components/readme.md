# deckgo-social-img



<!-- Auto Generated Below -->


## Properties

| Property          | Attribute           | Description                                                                               | Type     | Default     |
| ----------------- | ------------------- | ----------------------------------------------------------------------------------------- | -------- | ----------- |
| `height`          | `height`            | The social image width                                                                    | `string` | `'628px'`   |
| `imgSrc`          | `img-src`           | An optional image (https://....) that can for example be displayed as logo.               | `string` | `undefined` |
| `innerPadding`    | `inner-padding`     | A padding to create space around the text and the content                                 | `number` | `32`        |
| `padding`         | `padding`           | A padding to create space around the content                                              | `number` | `64`        |
| `rectColor`       | `rect-color`        | The color for rectangles                                                                  | `string` | `'#3dc2ff'` |
| `rectStrokeWidth` | `rect-stroke-width` | The width of the stroke of the rectangles                                                 | `number` | `5`         |
| `text`            | `text`              | The text to display (per default clamped with three dots "..." after some lines, see CSS) | `string` | `undefined` |
| `width`           | `width`             | The social image width                                                                    | `string` | `'1200px'`  |


## Methods

### `toBlob(type?: string) => Promise<Blob>`

Transform the rendered svg to an image

#### Returns

Type: `Promise<Blob>`




## Shadow Parts

| Part      | Description                                                                                  |
| --------- | -------------------------------------------------------------------------------------------- |
| `"image"` | The CSS pseudo-element to target the image displayed as a logo                               |
| `"img"`   |                                                                                              |
| `"text"`  | The CSS pseudo-element to target the paragraph rendered as a child of the SVG foreign object |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
