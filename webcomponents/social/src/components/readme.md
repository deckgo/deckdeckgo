# deckgo-social

The "Social" component helps you generate a social link to your Twitter, Dev, Medium, LinkedIn, GitHub accounts or a custom uri.

## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) social component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/social@latest/dist/deckdeckgo-social/deckdeckgo-social.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/social) using the following command:

```bash
npm install @deckdeckgo/social
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/social';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/social/dist/loader';
deckDeckGoElement();
```

## Usage

The "Social" Web Component could be integrated using the tag `<deckgo-social/>`.

```
<deckgo-social twitter="daviddalbusco">
  <img data-src="/assets/twitter.svg" slot="icon"/>
</deckgo-social>
```

#### Examples

Automatic text:

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
</deckgo-social>
```

Custom text:

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
    <span>A link to Twitter</span>
</deckgo-social>
```

Without icon:

```
<deckgo-social twitter="daviddalbusco">
</deckgo-social>
```

<!-- Auto Generated Below -->


## Properties

| Property   | Attribute  | Description                                                                                     | Type     | Default     |
| ---------- | ---------- | ----------------------------------------------------------------------------------------------- | -------- | ----------- |
| `dev`      | `dev`      | Your Dev username. It will be concatenated automatically with https://dev.to/                   | `string` | `undefined` |
| `fullUrl`  | `full-url` | In case you would like to provide the URI of your choice                                        | `string` | `undefined` |
| `github`   | `github`   | Your GitHub username. It will be concatenated automatically with https://github.com/            | `string` | `undefined` |
| `linkedin` | `linkedin` | Your LinkedIn username. It will be concatenated automatically with https://www.linkedin.com/in/ | `string` | `undefined` |
| `medium`   | `medium`   | Your Medium username. username will be replaced automatically from https://username.medium.com/ | `string` | `undefined` |
| `twitter`  | `twitter`  | Your Twitter username. It will be concatenated automatically with https://twitter.com/          | `string` | `undefined` |


## Methods

### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot     | Description                                     |
| -------- | ----------------------------------------------- |
|          | A custom text to be displayed                   |
| `"icon"` | An icon to be displayed next to the social link |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
