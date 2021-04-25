# deckgo-demo


## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/demo@latest/dist/deckdeckgo-demo/deckdeckgo-demo.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/demo) using the following command:

```bash
npm install @deckdeckgo/demo
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/demo';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/demo/dist/loader';
deckDeckGoElement();
```

## Usage

The "Demo" Web Component could be integrated using the tag `<deckgo-demo/>`.

```
<deckgo-demo src="https://deckdeckgo.app"></deckgo-demo>
```


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
