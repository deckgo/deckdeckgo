# deckgo-word-cloud

Write and render word clouds.

This component is using [D3](https://d3js.org/) and [d3-cloud](https://github.com/jasondavies/d3-cloud) to render these.

## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) word cloud component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/word-cloud@latest/dist/deckdeckgo-word-cloud/deckdeckgo-word-cloud.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/word-cloud) using the following command:

```bash
npm install @deckdeckgo/word-cloud
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/word-cloud';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/word-cloud/dist/loader';
deckDeckGoElement();
```

## Usage

The "Word Cloud" Web Component could be integrated using the tag `<deckgo-word-cloud/>`.

```
  <deckgo-word-cloud>
     <code slot="words">
        Each Word entered it will become part of the cloud
     </code>
  </deckgo-word-cloud>
```

Becomes editable by setting the "editable" property to "true".

```
  <deckgo-word-cloud editable="true">
    <code slot="words"></code>
  </deckgo-word-cloud>
```

## Styling

For each word, a style attribute `--deckgo-word-count-fill-color-${index}` will be generated. Replace `${index}` with a number such as `0` or `10`.

<!-- Auto Generated Below -->


## Properties

| Property       | Attribute       | Description                                                                                    | Type      | Default |
| -------------- | --------------- | ---------------------------------------------------------------------------------------------- | --------- | ------- |
| `editable`     | `editable`      | To set the component has being editable (contenteditable will be applied on the slot on click) | `boolean` | `false` |
| `marginBottom` | `margin-bottom` | Margin bottom in pixels                                                                        | `number`  | `32`    |
| `marginLeft`   | `margin-left`   | Margin left in pixels                                                                          | `number`  | `32`    |
| `marginRight`  | `margin-right`  | Margin right in pixels                                                                         | `number`  | `32`    |
| `marginTop`    | `margin-top`    | Margin top in pixels                                                                           | `number`  | `32`    |


## Events

| Event                | Description                         | Type                       |
| -------------------- | ----------------------------------- | -------------------------- |
| `wordCloudDidChange` | Emit the host element when modified | `CustomEvent<HTMLElement>` |


## Methods

### `lazyLoadContent() => Promise<void>`

Call the load and resize of the word cloud

#### Returns

Type: `Promise<void>`




## Slots

| Slot      | Description                            |
| --------- | -------------------------------------- |
| `"words"` | The list of words to render in a cloud |


## CSS Custom Properties

| Name                             | Description                                                                     |
| -------------------------------- | ------------------------------------------------------------------------------- |
| `--deckgo-word-cloud-empty-text` | Place holder in case editable is set to true @default "Click to add your words" |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
