# Installation

To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI and the starter kit as described in the [previous chapter](/docs/introduction).

However, the [DeckDeckGo] core component, could be installed in any project too.

If you wish to do so, use it directly in your project from a CDN, using a simple script include, or install it from [npm](https://www.npmjs.com/package/@deckdeckgo/core).

## Table of contents

- [Using DeckDeckGo from a CDN](#app-installation-using-deckdeckgo-from-a-cdn)
- [Install DeckDeckGo from npm](#app-installation-install-deckdeckgo-from-npm)
- [Framework integration](#app-installation-framework-integration)

## Using DeckDeckGo from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/core@latest/dist/deckdeckgo-core/deckdeckgo-core.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/core@latest/dist/deckdeckgo-core/deckdeckgo-core.js"></script>
```

## Install DeckDeckGo from NPM

Install [DeckDeckGo] in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/core) using the following command:

```bash
npm install @deckdeckgo/core
```

## Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

[DeckDeckGo]: https://deckdeckgo.com