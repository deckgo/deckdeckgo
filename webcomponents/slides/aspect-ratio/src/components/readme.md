# deckgo-slide-aspect-ratio

The "Aspect Ratio" slide is a template which preserves the content ratio regardless of the devices.

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-aspect-ratio@latest/dist/deckdeckgo-slide-aspect-ratio/deckdeckgo-slide-aspect-ratio.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-aspect-ratio) run the following command:

```bash
npm install @deckdeckgo/slide-aspect-ratio
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-aspect-ratio';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-aspect-ratio/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Aspect Ratio" slide's Web Component could be integrated using the tag `<deckgo-slide-aspect-ratio/>`.

```
<deckgo-deck>
  <deckgo-slide-aspect-ratio>
    <h1 style="position: absolute; top: 50%; left: 25%">An element</h1>
    <p style="position: absolute; top: 4%; left: 5%">
      Another element
    </p>
  </deckgo-slide-aspect-ratio>
</deckgo-deck>
```

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                              | Type      | Default  |
| ------------------ | ------------------- | -------------------------------------------------------------------------------------------------------- | --------- | -------- |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true      | `boolean` | `false`  |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true  | `boolean` | `false`  |
| `editable`         | `editable`          | Per default point-events are set to none for this template making it read-only respectively not editable | `boolean` | `false`  |
| `grid`             | `grid`              | Display a grid behind the content. Note that the grid would only be display if not fullscreen            | `boolean` | `false`  |
| `ratio`            | `ratio`             | The aspect ratio of the displayed content. Per default 16 being the width and 9 the height               | `number`  | `16 / 9` |


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



### `getContainer() => Promise<HTMLDivElement>`



#### Returns

Type: `Promise<HTMLDivElement>`



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

| Slot           | Description                                     |
| -------------- | ----------------------------------------------- |
|                | Multiple elements to be displayed in the slide  |
| `"actions"`    | Custom actions for this slide                   |
| `"background"` | A custom background for this slide              |
| `"bottom"`     | An optional text to be displayed at the bottom  |
| `"code"`       | A block of code to highlight if src is not used |
| `"footer"`     | A custom footer for this slide                  |
| `"header"`     | A custom header for this slide                  |
| `"notes"`      | Some notes related to this slide                |
| `"top"`        | An optional text to be displayed at the top     |


## CSS Custom Properties

| Name                           | Description                                                                                                                                                                                   |
| ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `--background`                 | background                                                                                                                                                                                    |
| `--color`                      | color                                                                                                                                                                                         |
| `--overflow`                   | overflow of the slide @default hidden                                                                                                                                                         |
| `--slide-grid-background`      | The default grid color @default linear-gradient(to bottom, rgba(0, 0, 0, 0) 98%, rgba(110, 109, 111, 0.4) 98%), linear-gradient(to right, rgba(0, 0, 0, 0) 98%, rgba(110, 109, 111, 0.4) 98%) |
| `--slide-grid-background-size` | The default size of each squares of the grid @default 2em 2em                                                                                                                                 |
| `--slide-padding-bottom`       | Padding bottom of the slide @default 64px and 32px on wider screen                                                                                                                            |
| `--slide-padding-end`          | Padding right of the slide @default 64px and 32px on wider screen                                                                                                                             |
| `--slide-padding-start`        | Padding left of the slide @default 64px and 32px on wider screen                                                                                                                              |
| `--slide-padding-top`          | Padding top of the slide @default 64px and 32px on wider screen                                                                                                                               |
| `--slide-user-select`          | user select @default none                                                                                                                                                                     |
| `--zIndex`                     | z-index @default 1                                                                                                                                                                            |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
