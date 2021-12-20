# deckgo-social-img



<!-- Auto Generated Below -->


## Properties

| Property          | Attribute           | Description                                                                                                                                                                         | Type     | Default           |
| ----------------- | ------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------- | ----------------- |
| `background`      | `background`        | A background color for the all social image, the all SVG                                                                                                                            | `string` | `'#ffffff'`       |
| `height`          | `height`            | The social image width                                                                                                                                                              | `string` | `'628px'`         |
| `imgMimeType`     | `img-mime-type`     | The mime type of the image. Default 'image/svg+xml'                                                                                                                                 | `string` | `'image/svg+xml'` |
| `imgSrc`          | `img-src`           | An optional image (https://....) that can for example be displayed as logo. Note: it will be fetched and transformed to base64. The SVG won't be rendered until the logo is loaded. | `string` | `undefined`       |
| `innerPadding`    | `inner-padding`     | A padding to create space around the text and the content                                                                                                                           | `number` | `32`              |
| `padding`         | `padding`           | A padding to create space around the content                                                                                                                                        | `number` | `64`              |
| `rectBackground`  | `rect-background`   | The background color for the rectangle at the top                                                                                                                                   | `string` | `'#ffffff'`       |
| `rectColor`       | `rect-color`        | The color for rectangles                                                                                                                                                            | `string` | `'#3dc2ff'`       |
| `rectRx`          | `rect-rx`           | A x-axis radius for rectangles                                                                                                                                                      | `string` | `'0'`             |
| `rectRy`          | `rect-ry`           | A y-axis radius for rectangles                                                                                                                                                      | `string` | `'0'`             |
| `rectStrokeWidth` | `rect-stroke-width` | The width of the stroke of the rectangles around the text                                                                                                                           | `number` | `5`               |
| `text`            | `text`              | The text to display (per default clamped with three dots "..." after some lines, see CSS)                                                                                           | `string` | `undefined`       |
| `width`           | `width`             | The social image width                                                                                                                                                              | `string` | `'1200px'`        |


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
