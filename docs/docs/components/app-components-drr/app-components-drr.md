# Drag, Resize and Rotate

The "Drag, Resize and Rotate" is a Web Component to drag, resize and rotate any element.

## Table of contents

- [Showcase](#app-components-drag-resize-rotate-showcase)
- [Installation](#app-components-drag-resize-rotate-installation)
  - [Using from a CDN](#app-components-drag-resize-rotate-from-a-cdn)
  - [Install from NPM](#app-components-drag-resize-rotate-from-npm)
  - [Framework integration](#app-components-drag-resize-rotate-framework-integration)
- [Usage](#app-components-drag-resize-rotate-usage)
  - [Slots](#app-components-drag-resize-rotate-slots)
  - [Attributes](#app-components-drag-resize-rotate-attributes)
  - [Event Listeners](#app-components-drag-resize-rotate-event-listeners)
  - [Theming](#app-components-drag-resize-rotate-theming)
  - [Events](#app-components-drag-resize-rotate-events)
- [Examples](#app-components-drag-resize-rotate-examples)

## Showcase

<div style={{position: 'relative', width: '200px', height: '200px', background: '#cccccc'}}>
  <deckgo-drr style={{'--width': '20%', '--height': '10%', '--top': '5%', '--left': '10%', '--rotate': '45deg'}}>
    <div style={{background: '#FF0000'}}></div>
  </deckgo-drr>
</div>

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

### Slots

The default slot is mandatory.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute | Type                                | Default      | Description                                                                                                                                                                                         |
| --------- | ----------------------------------- | ------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| unit      | 'percentage', 'viewport' or 'px'    | 'percentage' | The component could be use with percentage, viewport (vw/vh) or pixels (px) units. All relative to the container.                                                                                   |
| resize    | boolean                             | true         | Allow or not the resize actions                                                                                                                                                                     |
| drag      | 'x-axis', 'y-axis', 'all' or 'none' | 'all'        | Allow the component to be dragged in which direction                                                                                                                                                |
| rotation  | boolean                             | true         | Allow or not the rotation of the element                                                                                                                                                            |
| text      | boolean                             | false        | To be used if your slotted element is to be defined as `contentEditable`. Useful for text edition. Note that if turns to `true`, the property `resize` is going to be set to `false` automatically. |

### Event listeners

The actions are triggered through interaction with the mouse or touch.

If for some reason you would like to block the actions and listeners, set the style `pointer-events` of the container to `none` and also add it a class `deckgo-read-only`.

```
<div class="deckgo-read-only" style="pointer-events: none">
<deckgo-drr style="--width: 10%; --height: 10%; --top: 25%; --left: 10%;">
  <p>Element will not be selectable nor draggable, resizable and rotatable</p>
</deckgo-drr>
</did>
```

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                                        | Default                 | Note                                                                     |
| ---------------------------------------------------- | ----------------------- | ------------------------------------------------------------------------ |
| --width                                              |                         | The default width. Will be overwritten if component is modified.         |
| --height                                             |                         | The default height. Will be overwritten if component is modified.        |
| --top                                                |                         | The default top position. Will be overwritten if component is modified.  |
| --left                                               |                         | The default left position. Will be overwritten if component is modified. |
| --rotate                                             |                         | The default rotate angle. Will be overwritten if component is rotated.   |
| --deckgo-drr-user-select                             | 'none'                  | The user selection on the host component                                 |
| --deckgo-drr-border                                  | 1px solid #3880f7f      | A border around the component if selected                                |
| --deckgo-drr-anchor-width                            | 16px                    | The default width of an anchor                                           |
| --deckgo-drr-anchor-height                           | 16px                    | The default height of an anchor                                          |
| --deckgo-drr-anchor-padding-desktop                  | 16px                    | The default padding of an anchor on desktop                              |
| --deckgo-drr-anchor-padding-mobile                   |                         | The default padding of an anchor on touch devices                        |
| --deckgo-drr-anchor-border-radius                    | 50%                     | The default border radius of an anchor                                   |
| --deckgo-drr-anchor-background                       | #3880ff                 | The background of an anchor                                              |
| --deckgo-drr-anchor-border                           |                         | The border of an anchor                                                  |
| --deckgo-drr-rotate-anchor-width                     | 24px                    | The rotate block anchor width                                            |
| --deckgo-drr-rotate-anchor-height                    | 32px                    | The rotate block anchor height                                           |
| --deckgo-drr-rotate-anchor-action-width              | 16px                    | The rotate block anchor action width                                     |
| --deckgo-drr-rotate-anchor-action-height             | 16px                    | The rotate block anchor action height                                    |
| --deckgo-drr-rotate-anchor-action-border-radius      | 50%                     | The rotate block anchor action border radius                             |
| --deckgo-drr-rotate-anchor-action-background         |                         | The rotate block anchor action background                                |
| --deckgo-drr-rotate-anchor-action-border             | 1px solid #3880ff       | The rotate block anchor action border                                    |
| --deckgo-drr-rotate-anchor-presentation-height       | calc(100% - 16px - 1px) | The rotate block anchor presentation block height                        |
| --deckgo-drr-rotate-anchor-presentation-border-right | 1px solid #3880ff       | The rotate block anchor presentation block border right                  |

### Events

The `<deckgo-drr/>` component bubbles the following events:

#### Select

Emitted when the component is selected or unselected. It propagates the host component itself.

```
drrSelect(HTMLElement || undefined);
```

#### Change

Emitted when the component is modified respectively when the user stop interacting. It propagates the host component itself.

```
drrDidChange(HTMLElement);
```

## Examples

```
<deckgo-drr style="--width: 20%; --height: 10%; --top: 5%; --left: 10%; --rotate: 45deg;">
  <div style="background: red;"></div>
</deckgo-drr>

<deckgo-drr style="--width: 10%; --height: 20%; --top: 45%; --left: 8%;" drag="y-axis">
  <div style="background: yellow"></div>
</deckgo-drr>

<deckgo-drr style="--width: 10%; --height: 10%; --top: 25%; --left: 10%;" resize="false">
  <div style="background: green"></div>
</deckgo-drr>

<deckgo-drr style="--width: 10%; --height: 15%; --top: 25%; --left: 15%;" drag="none">
  <div style="background: greenyellow; border-radius: 50%;" slot></div>
</deckgo-drr>

<deckgo-drr unit="px" style="--width: 34px; --height: 20px; --top: 40px; --left: 25px;">
  <img src="https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg" style="display: none;" />
</deckgo-drr>

<deckgo-drr style="--width: 34%; --height: 20%; --top: 40%; --left: 25%;">
  <img src="https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg" style="display: none;" />
</deckgo-drr>
```

[deckdeckgo]: https://deckdeckgo.com
