# Installation

To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI and the starter kit as described in the [previous chapter](/docs/introduction).

However, the [DeckDeckGo] core component, could be installed in any project too.

If you wish to do so, use it directly in your project from a CDN, using a simple script include, or install it from [npm](https://www.npmjs.com/package/@deckdeckgo/core).

> Installing the core component as displayed below will "only" install the "engine" of [DeckDeckGo] respectively its core doesn't contains any slides.
>
> Splitting the core and the templates has for goal to reduce as much as possible the dependencies and amount of external code needed in your project. Therefore, it has for ultimate goal to unleash the best performances for your presentation.

## Table of contents

- [Using DeckDeckGo from a CDN](#app-installation-using-deckdeckgo-from-a-cdn)
- [Install DeckDeckGo from npm](#app-installation-install-deckdeckgo-from-npm)
- [Framework integration](#app-installation-framework-integration)

## Using DeckDeckGo from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] core from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/core@latest/dist/deckdeckgo/deckdeckgo.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/core@latest/dist/deckdeckgo/deckdeckgo.js"></script>
```

## Install DeckDeckGo from NPM

Install [DeckDeckGo] in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/core) using the following command:

```bash
npm install @deckdeckgo/core
```

## Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` the component:

### Import

```
import '@deckdeckgo/core';
```

### Loader

```
import { defineCustomElements as deckDeckGoElements } from '@deckdeckgo/core/dist/loader';
deckDeckGoElements(window);
```

[DeckDeckGo]: https://deckdeckgo.com