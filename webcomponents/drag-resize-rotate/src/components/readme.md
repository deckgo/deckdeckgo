# deckgo-drr

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/drag-resize-rotate@latest/dist/deckdeckgo-drag-resize-rotate/deckdeckgo-drag-resize-rotate.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/drag-resize-rotate
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/drag-resize-rotate';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/drag-resize-rotate/dist/loader';
deckDeckGoElement();
```

## Usage

The "Drag, Resize and Rotate" Web Component could be integrated using the tag `<deckgo-drr/>`.

```
<deckgo-drr style="--width: 10%; --height: 10%; --top: 25%; --left: 10%;">
  <div style="background: green"></div>
</deckgo-drr>
```

<!-- Auto Generated Below -->


## Properties

| Property   | Attribute  | Description                                                                                                                                                                                | Type                                      | Default        |
| ---------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------- | -------------- |
| `drag`     | `drag`     | Allow the component to be dragged in which direction                                                                                                                                       | `"all" \| "none" \| "x-axis" \| "y-axis"` | `'all'`        |
| `resize`   | `resize`   | Allow or not the resize actions                                                                                                                                                            | `boolean`                                 | `true`         |
| `rotation` | `rotation` | Allow or not the rotation of the element                                                                                                                                                   | `boolean`                                 | `true`         |
| `text`     | `text`     | To be used if your slotted element is to be defined as contentEditable. Useful for text edition. Note that if turns to true, the property resize is going to be set to false automatically | `boolean`                                 | `false`        |
| `unit`     | `unit`     | The component could be use with percentage, viewport (vw/vh) or pixels (px) units. All relative to the container                                                                           | `"percentage" \| "px" \| "viewport"`      | `'percentage'` |


## Events

| Event          | Description                                                                                                                 | Type                       |
| -------------- | --------------------------------------------------------------------------------------------------------------------------- | -------------------------- |
| `drrDidChange` | Emitted when the component is modified respectively when the user stop interacting. It propagates the host component itself | `CustomEvent<HTMLElement>` |
| `drrSelect`    | Emitted when the component is selected or unselected. It propagates the host component itself                               | `CustomEvent<HTMLElement>` |


## CSS Custom Properties

| Name                                                   | Description                                                                         |
| ------------------------------------------------------ | ----------------------------------------------------------------------------------- |
| `--deckgo-drr-anchor-background`                       | The background of an anchor @default #3880ff                                        |
| `--deckgo-drr-anchor-border`                           | The border of an anchor                                                             |
| `--deckgo-drr-anchor-border-radius`                    | The default border radius of an anchor @default 50%                                 |
| `--deckgo-drr-anchor-height`                           | The default height of an anchor @default 16px                                       |
| `--deckgo-drr-anchor-padding-desktop`                  | The default padding of an anchor on desktop @default 16px                           |
| `--deckgo-drr-anchor-padding-mobile`                   | The default padding of an anchor on touch devices                                   |
| `--deckgo-drr-anchor-width`                            | The default width of an anchor @default 16px                                        |
| `--deckgo-drr-border`                                  | A border around the component if selected @default 1px solid #3880ff                |
| `--deckgo-drr-rotate-anchor-action-background`         | The rotate block anchor action background                                           |
| `--deckgo-drr-rotate-anchor-action-border`             | The rotate block anchor action border @default 1px solid #3880ff                    |
| `--deckgo-drr-rotate-anchor-action-border-radius`      | The rotate block anchor action border radius @default 50%                           |
| `--deckgo-drr-rotate-anchor-action-height`             | The rotate block anchor action height @default 16px                                 |
| `--deckgo-drr-rotate-anchor-action-width`              | The rotate block anchor action width @default 16px                                  |
| `--deckgo-drr-rotate-anchor-height`                    | The rotate block anchor action height @default 32px                                 |
| `--deckgo-drr-rotate-anchor-presentation-border-right` | The rotate block anchor presentation block border right @default 1px solid #3880ff  |
| `--deckgo-drr-rotate-anchor-presentation-height`       | The rotate block anchor presentation block height @default calc(100% - 16px - 1px)  |
| `--deckgo-drr-rotate-anchor-width`                     | The rotate block anchor action width @default 24px                                  |
| `--deckgo-drr-user-select`                             | The user selection on the host component @default none                              |
| `--height`                                             | The default height. Will be overwritten if component is modified                    |
| `--left`                                               | The default left position. Will be overwritten if component is modified             |
| `--rotate`                                             | The default rotate angle. Will be overwritten if component is rotated @default 0deg |
| `--top`                                                | The default top position. Will be overwritten if component is modified              |
| `--width`                                              | The default width. Will be overwritten if component is modified                     |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
