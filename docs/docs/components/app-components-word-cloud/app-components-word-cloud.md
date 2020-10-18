# Word Cloud

Write and render word clouds.

This component is using [D3](https://d3js.org/) and [d3-cloud](https://github.com/jasondavies/d3-cloud) to render these.

## Table of contents

- [Showcase](#app-components-word-cloud-showcase)
- [Installation](#app-components-word-cloud-installation)
  - [Using from a CDN](#app-components-word-cloud-from-a-cdn)
  - [Install from NPM](#app-components-word-cloud-from-npm)
  - [Framework integration](#app-components-word-cloud-framework-integration)
- [Usage](#app-components-word-cloud-usage)
  - [Slots](#app-components-word-cloud-slots)
  - [Attributes](#app-components-word-cloud-attributes)
  - [Events](#app-components-word-cloud-events)
  - [Theming](#app-components-word-cloud-theming)

## Showcase

<div style={{position: 'relative', width: '560px', height: '560px'}}>
      <deckgo-word-cloud>
        <code slot="words"
          >How the Word Cloud Generator Works The layout algorithm for positioning words without overlap is available on GitHub under an open source license as
          d3-cloud. Note that this is the only the layout algorithm and any code for converting text into words and rendering the final output requires
          additional development.
        </code>
      </deckgo-word-cloud>
</div>

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] word cloud component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/word-cloud@latest/dist/deckdeckgo-word-cloud/deckdeckgo-word-cloud.esm.js"></script>
<script nomodule="" src="https://unpkg.com/browse/@deckdeckgo/word-cloud@latest/dist/deckdeckgo-word-cloud/deckdeckgo-word-cloud.js"></script>
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

### Slots

The words have to be provided using the slot `words`.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute    | Type    | Default | Description                                                                                          |
| ------------ | ------- | ------- | ---------------------------------------------------------------------------------------------------- |
| editable     | boolean | `false` | To set the component has being editable (`contenteditable` will be applied on the `slot` on `click`) |
| marginTop    | number  | 32      | Margin top in pixels                                                                                 |
| marginBottom | number  | 32      | Margin bottom in pixels                                                                              |
| marginLeft   | number  | 32      | Margin left in pixels                                                                                |
| marginRight  | number  | 32      | Margin right in pixels                                                                               |

### Events

The `<deckgo-word-cloud/>` component triggers the following event.

| Event                | Description                          | Type                       |
| -------------------- | ------------------------------------ | -------------------------- |
| `wordCloudDidChange` | Emit the host element when modified. | `CustomEvent<HTMLElement>` |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                  | Default                   | Note                                             |
| ------------------------------ | ------------------------- | ------------------------------------------------ |
| --deckgo-word-cloud-empty-text | "Click to add your words" | Place holder in case `editable` is set to `true` |

[deckdeckgo]: https://deckdeckgo.com
