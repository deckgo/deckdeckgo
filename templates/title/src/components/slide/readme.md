# deckgo-slide-title

The "Title" slide is a simple slide which display its title and content centered in the middle of the page.

## Installation

This template could be added to your presentation using the following methods.

> This template is included per default in our Developer Kit

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-title@latest/dist/deckdeckgo-slide-title/deckdeckgo-slide-title.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-title) run the following command:

```bash
npm install @deckdeckgo/slide-title
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-title';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-title/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Title" slide's Web Component could be integrated using the tag `<deckgo-slide-title/>`.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <p slot="content">
      Hello World 🚀
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

<!-- Auto Generated Below -->


## Properties

| Property           | Attribute           | Description                                                                                             | Type      | Default |
| ------------------ | ------------------- | ------------------------------------------------------------------------------------------------------- | --------- | ------- |
| `customActions`    | `custom-actions`    | If you provide actions for the all deck but, a specific one for this slide, set this option to true     | `boolean` | `false` |
| `customBackground` | `custom-background` | If you define a background for the all deck but, a specific one for this slide, set this option to true | `boolean` | `false` |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


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
| `"content"`    | A content                          |
| `"footer"`     | A custom footer for this slide     |
| `"header"`     | A custom header for this slide     |
| `"notes"`      | Some notes related to this slide   |
| `"title"`      | A title                            |


## CSS Custom Properties

| Name                     | Description                                                        |
| ------------------------ | ------------------------------------------------------------------ |
| `--background`           | background                                                         |
| `--color`                | color                                                              |
| `--overflow`             | overflow of the slide @default hidden                              |
| `--slide-padding-bottom` | Padding bottom of the slide @default 64px and 32px on wider screen |
| `--slide-padding-end`    | Padding right of the slide @default 64px and 32px on wider screen  |
| `--slide-padding-start`  | Padding left of the slide @default 64px and 32px on wider screen   |
| `--slide-padding-top`    | Padding top of the slide @default 64px and 32px on wider screen    |
| `--slide-user-select`    | user select @default none                                          |
| `--zIndex`               | z-index @default 1                                                 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
