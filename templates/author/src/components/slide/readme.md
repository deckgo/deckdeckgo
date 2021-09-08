# deckgo-slide-author

The "Author" slide lets you introduce the author of the presentation.

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-author@latest/dist/deckdeckgo-slide-author/deckdeckgo-slide-author.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-author) run the following command:

```bash
npm install @deckdeckgo/slide-author
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-author';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-author/dist/loader';
deckDeckGoSlideElement();
```

### Social component

This template relies on the `@deckdeckgo/social` component without any explicit dependency. Therefore it should also be installed.

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                                                                                           | Type                            | Default     |
| ------------------ | ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------- | ----------- |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true                                                                   | `boolean`                       | `false`     |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true                                                               | `boolean`                       | `false`     |
| `imgAlt`           | `img-alt`           | An optional accessibility alt for the image.                                                                                                                          | `string`                        | `undefined` |
| `imgMode`          | `img-mode`          | The design to be applied to the image. cover fits the image to the start pane, circle displays it in a circle and none in case you would not like to display an image | `"circle" \| "cover" \| "none"` | `'cover'`   |
| `imgSrc`           | `img-src`           | An image URI, for example a picture of the author. Note: this image will be displayed as a circle.                                                                    | `string`                        | `undefined` |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(_enter: boolean, _reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot            | Description                        |
| --------------- | ---------------------------------- |
| `"actions"`     | Custom actions for this slide      |
| `"author"`      | Author of the slides               |
| `"background"`  | A custom background for this slide |
| `"footer"`      | A custom footer for this slide     |
| `"header"`      | A custom header for this slide     |
| `"notes"`       | Some notes related to this slide   |
| `"social-link"` | Several optional social links      |
| `"title"`       | A title                            |


## CSS Custom Properties

| Name                                 | Description                                                                       |
| ------------------------------------ | --------------------------------------------------------------------------------- |
| `--background`                       | background                                                                        |
| `--color`                            | color                                                                             |
| `--overflow`                         | overflow of the slide @default hidden                                             |
| `--slide-author-background-end`      | Right pane background                                                             |
| `--slide-author-background-start`    | Left pane background                                                              |
| `--slide-author-color-end`           | Right pane color                                                                  |
| `--slide-author-color-start`         | Left pane color                                                                   |
| `--slide-author-img-border`          | The border of the image of the left pane (only apply if circle mode is specified) |
| `--slide-author-img-size`            | The size of the image of the left pane @default 80%                               |
| `--slide-author-padding-bottom`      | Padding bottom of the slide @default 64px and 32px on wider screen                |
| `--slide-author-padding-end`         | Padding right of the slide @default 64px and 32px on wider screen                 |
| `--slide-author-padding-start`       | Padding left of the slide @default 64px and 32px on wider screen                  |
| `--slide-author-padding-top`         | Padding top of the slide @default 64px and 32px on wider screen                   |
| `--slide-author-social-link-padding` | Padding for the social links @default 8px                                         |
| `--slide-user-select`                | user select @default none                                                         |
| `--zIndex`                           | z-index @default 1                                                                |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
