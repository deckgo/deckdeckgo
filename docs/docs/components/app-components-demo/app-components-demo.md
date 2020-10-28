# Demo

Display your apps or websites inside an Android or iOS frame.

It is largely inspired and based on the awesome [work](https://github.com/ionic-team/ionic-docs/tree/c5a624ac35d5285b871e7d8513d3849bdea63271/src/components/demo) of the [Ionic](https://ionicframework.com/) team.

## Table of contents

- [Showcase](#app-components-demo-showcase)
- [Installation](#app-components-demo-installation)
  - [Using from a CDN](#app-components-demo-from-a-cdn)
  - [Install from NPM](#app-components-demo-from-npm)
  - [Framework integration](#app-components-demo-framework-integration)
- [Usage](#app-components-demo-usage)
  - [Slots](#app-components-demo-slots)
  - [Attributes](#app-components-demo-attributes)
  - [Methods](#app-components-demo-methods)
  - [Theming](#app-components-demo-theming)
  - [Sizing](#app-components-demo-sizing)

## Showcase

<div style={{position: 'relative'}}>
  <deckgo-demo style={{'width': '304px', 'height': '704px'}} src="https://deckdeckgo.com" instant={true}>
  </deckgo-demo>
</div>

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

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

### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute   | Type          | Default | Description                                                                                                       |
| ----------- | ------------- | ------- | ----------------------------------------------------------------------------------------------------------------- |
| src         | string        |         | The source Url of your application or website. This will be used as `src` attribute of the encapsulated `iframe`. |
| frame-title | string        |         | A title for the frame, could be use for accessibility reason.                                                     |
| mode        | 'md' or 'ios' | 'md'    | The type of device frame. `md` for Android, `ios` for iPhone.                                                     |
| instant     | boolean       | false   | In case you would like to load the frame as soon as the component is loaded.                                      |

Per default the `iframe` is not be loaded (expect if you specify `instant` to `true`). Therefore it's up to you to call the method `lazyLoadContent` to create it. The reason behind this decision is allowing you to lazy load your content.

### Methods

The `<deckgo-demo/>` component exposes the following methods:

#### Lazy load the iframe

```
lazyLoadContent(): Promise<void>
```

#### Refresh iframe size and reload content

```
updateIFrame(): Promise<void>
```

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                        | Default | Note                                                                        |
| ------------------------------------ | ------- | --------------------------------------------------------------------------- |
| --deckgo-demo-placeholder-background | #323639 | A background color for the content of the device until the frame is loaded. |

### Sizing

On load and on window resize, the component will take care of resizing itself. It will compute the `width` and `height` of the host element to apply these to its content, to the shadowed iframe.

[deckdeckgo]: https://deckdeckgo.com
