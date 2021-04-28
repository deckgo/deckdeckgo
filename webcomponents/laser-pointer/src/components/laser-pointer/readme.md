# deckgo-flashlight

Display the mouse cursor as a laser pointer.

## Installation

This component can be added to your web application with following methods.

> If you are using our developer kit to create a presention, this component is already included

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) charts from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/laser-pointer@latest/dist/deckdeckgo-laser-pointer/deckdeckgo-laser-pointer.esm.js"></script>
```

### Install from NPM

Install [DeckDeckGo](https://deckdeckgo.com) charts in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/laser-pointer) using the following command:

```bash
npm install @deckdeckgo/laser-pointer
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/laser-pointer';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/laser-pointer/dist/loader';
deckDeckGoElement();
```

<!-- Auto Generated Below -->


## Properties

| Property   | Attribute   | Description                                     | Type      | Default |
| ---------- | ----------- | ----------------------------------------------- | --------- | ------- |
| `animateB` | `animate-b` | Decrease RGB blue color with life span          | `boolean` | `true`  |
| `animateG` | `animate-g` | Decrease RGB green color with life span         | `boolean` | `false` |
| `animateR` | `animate-r` | Decrease RGB red color with life span           | `boolean` | `true`  |
| `b`        | `b`         | The RGB blue color (which will fade with time)  | `number`  | `255`   |
| `g`        | `g`         | The RGB green color (which will fade with time) | `number`  | `194`   |
| `r`        | `r`         | The RGB red color                               | `number`  | `61`    |


## CSS Custom Properties

| Name                     | Description                                                  |
| ------------------------ | ------------------------------------------------------------ |
| `--laser-pointer-zindex` | z-index property when laser pointer is displayed. @default 2 |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
