# deckgo-slide-split

The "Split" slide is a simple slide which display two panes on the page.

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-split@latest/dist/deckdeckgo-slide-split/deckdeckgo-slide-split.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-split) run the following command:

```bash
npm install @deckdeckgo/slide-split
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-split';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-split/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Split" slide's Web Component could be integrated using the tag `<deckgo-slide-split/>`.

```
<deckgo-deck>
  <deckgo-slide-split>
    <h1 slot="title">Two columns subject</h1>
    <p slot="start">
      The content you want to display on the left side of the page
    </p>
    <p slot="end">
      The content you want to display on the right side of the page
    </p>
  </deckgo-slide-split>
</deckgo-deck>
```

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                             | Type                  | Default     |
| ------------------ | ------------------- | ------------------------------------------------------------------------------------------------------- | --------------------- | ----------- |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true     | `boolean`             | `false`     |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true | `boolean`             | `false`     |
| `type`             | `type`              | Set to "demo" if you use such component in one of the start or end section                              | `"default" \| "demo"` | `'default'` |
| `vertical`         | `vertical`          | Split the slide horizontally (false) or vertically (true)                                               | `boolean`             | `false`     |


## Events

| Event          | Description | Type                |
| -------------- | ----------- | ------------------- |
| `slideDidLoad` |             | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(enter: boolean, reveal: boolean) => Promise<boolean>`



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

| Slot           | Description                        |
| -------------- | ---------------------------------- |
| `"actions"`    | Custom actions for this slide      |
| `"background"` | A custom background for this slide |
| `"end"`        | The end element (split)            |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"start"`      | The start element (split)          |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                             | Description                                                        |
| -------------------------------- | ------------------------------------------------------------------ |
| `--background`                   | background                                                         |
| `--color`                        | color                                                              |
| `--overflow`                     | overflow of the slide @default hidden                              |
| `--slide-split-background-end`   | end background                                                     |
| `--slide-split-background-start` | start background                                                   |
| `--slide-split-color-end`        | end color                                                          |
| `--slide-split-color-start`      | start color                                                        |
| `--slide-split-display-end`      | end display @default flex                                          |
| `--slide-split-display-start`    | start display @default flex                                        |
| `--slide-split-padding-bottom`   | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-split-padding-end`      | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-split-padding-start`    | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-split-padding-top`      | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-split-title-display`    | title display @default none                                        |
| `--slide-user-select`            | user select @default none                                          |
| `--zIndex`                       | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
