# deckgo-social-img

Generate dynamically a social image with a text and an optional logo.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/social-img@latest/dist/social-img/social-img.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/social-img) using the following command:

```bash
npm install @deckdeckgo/social-img
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/social-img';
```

#### Loader

```
import { defineCustomElements } from '@deckdeckgo/social-img/dist/loader';
defineCustomElements();
```

### Usage

```
<deckgo-social-img
      text="Lorem ipsum dolor sit amet."
      img-src="https://deckdeckgo-studio-staging.web.app/assets/icons/deckdeckgo.svg"
    >
    </deckgo-social-img>
```

### Notes

- the text to display is per default clamped with three dots "..." after three lines.
- the logo, `img-src`, needs to support CORS and will be fetched before rendering the social image

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
| `rectRx`          | `rect-rx`           | A x-axis radius for rectangles                                                                                                                                                      | `number` | `0`               |
| `rectRy`          | `rect-ry`           | A y-axis radius for rectangles                                                                                                                                                      | `number` | `0`               |
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


## CSS Custom Properties

| Name                 | Description                               |
| -------------------- | ----------------------------------------- |
| `--text-box-orient`  | Text -webkit-box-orient @default vertical |
| `--text-color`       | Text color                                |
| `--text-display`     | Text display @default -webkit-box         |
| `--text-font-family` | Text font-family @default inherit         |
| `--text-font-size`   | Text font-size @default 128px             |
| `--text-font-weight` | Text font-weight @default 700             |
| `--text-line-clamp`  | Text -webkit-line-clamp @default 3        |
| `--text-margin`      | Text margin @default 0                    |
| `--text-overflow`    | Text overflow @default hidden             |
| `--text-padding`     | Text padding @default 0                   |
| `--text-zindex`      | Text z-index @default 1                   |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
