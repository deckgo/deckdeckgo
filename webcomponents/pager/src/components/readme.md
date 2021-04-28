# deckgo-pager

A pager for the progression of the presentation can optionally be displayed in form of a progress circle bar.

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

<!-- Auto Generated Below -->


## Events

| Event        | Description                              | Type                |
| ------------ | ---------------------------------------- | ------------------- |
| `pagerClick` | Emitted when the user click on the pager | `CustomEvent<void>` |


## CSS Custom Properties

| Name                              | Description                                                                |
| --------------------------------- | -------------------------------------------------------------------------- |
| `--pager-background`              | circle background stroke @default #eee                                     |
| `--pager-color`                   | circle stroke @default black                                               |
| `--pager-margin-bottom`           | margin-bottom @default 8px                                                 |
| `--pager-margin-end`              | margin-right @default 32px                                                 |
| `--pager-margin-start`            | margin-left @default 32px                                                  |
| `--pager-margin-top`              | margin-top @default 8px                                                    |
| `--pager-position-left`           | The left attribute of the absolute positioning of the pager over the deck  |
| `--pager-position-right`          | The right attribute of the absolute positioning of the pager over the deck |
| `--pager-size`                    | Height and width @default 48px                                             |
| `--pager-stroke-inner-width`      | stroker inner width @default 2.2                                           |
| `--pager-stroke-outer-width`      | circle outer stroke-width @default 2.2                                     |
| `--pager-text-color`              | text color @default black                                                  |
| `--pager-text-percentage-display` | display percentage / progression @default none                             |
| `--pager-text-size`               | font-size @default 0.5em                                                   |
| `--pager-text-slides-display`     | display page index @default none                                           |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
