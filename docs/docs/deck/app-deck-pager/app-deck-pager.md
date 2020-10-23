# Pager

A pager for the progression of the presentation can optionally be displayed in form of a progress circle bar.

- [Limitation](#app-deck-pager-limitation)
- [Installation](#app-deck-pager-installation)
  - [Using from a CDN](#app-deck-pager-install-from-a-cdn)
  - [Install from NPM](#app-deck-pager-install-from-npm)
  - [Framework integration](#app-deck-pager-framework-integration)
- [Usage](#app-deck-pager-usge)
- [Customization](#app-deck-pager-customization)
- [Events](#app-deck-pager-events)

## Limitation

This pager only works in a single page presentation because it listens to events emitted by the deck on the `document` level.

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] pager from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/pager@latest/dist/deckdeckgo-pager/deckdeckgo-pager.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/pager) using the following command:

```bash
npm install @deckdeckgo/pager
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/pager';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/pager/dist/loader';
deckDeckGoElement();
```

## Usage

In order to the pager to your presentation, provide it as last child of your deck using the slot `pager`.

```
<deckgo-deck>
    <deckgo-pager slot="pager">
    </deckgo-pager>
</deckgo-deck>
```

## Customization

The following options are available to style the pager:

| CSS4 variable                   | Default | Description                                                                  |
| ------------------------------- | ------- | ---------------------------------------------------------------------------- |
| --pager-size                    | 56px    |                                                                              |
| --pager-margin-top              | 8px     |                                                                              |
| --pager-margin-end              | 8px     |                                                                              |
| --pager-margin-bottom           | 8px     |                                                                              |
| --pager-margin-start            | 8px     |                                                                              |
| --pager-background              | #eee    |                                                                              |
| --pager-text-color              | #4c8dff |                                                                              |
| --pager-text-size               | 0.5em   |                                                                              |
| --pager-stroke-outer-width      | 2.8     |                                                                              |
| --pager-stroke-inner-width      | 1.8     |                                                                              |
| --pager-text-percentage-display | none    | Set to `block` to display a progression with percentage (for example: 35%)   |
| --pager-text-slides-display     | none    | Set to `block` to display a progression as slides' count (for example: 2/15) |
| --pager-position-left           |         | The left attribute of the absolute positioning of the pager over the deck    |
| --pager-position-right          |         | The right attribute of the absolute positioning of the pager over the deck   |

Note: of course if you would display both `--pager-text-percentage-display` and `--pager-text-slides-display` it would be a weird display, use just one at once.

# Events

In case you would like to hook on the pager click, it triggers the following event:

| Event      | Emitted value | Description                               |
| ---------- | ------------- | ----------------------------------------- |
| pagerClick |               | Emitted when the user click on the pager. |

[deckdeckgo]: https://deckdeckgo.com
