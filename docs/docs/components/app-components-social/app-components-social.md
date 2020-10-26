# Social

The "Social" component helps you generate a social link to your Twitter, Dev, Medium, LinkedIn, GitHub accounts or a custom uri.

## Table of contents

- [Showcase](#app-components-social-showcase)
- [Installation](#app-components-social-installation)
  - [Using from a CDN](#app-components-social-install-from-a-cdn)
  - [Install from NPM](#app-components-social-install-from-npm)
  - [Framework integration](#app-components-social-framework-integration)
- [Usage](#app-components-social-usage)
  - [Slots](#app-components-social-slots)
  - [Attributes](#app-components-social-attributes)
  - [Examples](#app-components-social-examples)

## Showcase

<p>
  <deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon></deckgo-social>
</p>

<p>
  <deckgo-social github="deckgo/deckdeckgo"><ion-icon slot="icon" name="logo-github"></ion-icon> DeckDeckGo on Github</deckgo-social>
</p>

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] social component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/social@latest/dist/deckdeckgo-social/deckdeckgo-social.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/social@latest/dist/deckdeckgo-social/deckdeckgo-social.js"></script>
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

### Slots

The slot `icon` and the text are both optional.

If you don't provide a text, component will renders a corresponding text for you.

#### Examples

Automatic text:

<p>
  <deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon></deckgo-social>
</p>

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
</deckgo-social>
```

Custom text:

<p>
  <deckgo-social twitter="daviddalbusco"><ion-icon slot="icon" name="logo-twitter"></ion-icon><span>A link to Twitter</span></deckgo-social>
</p>

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
    <span>A link to Twitter</span>
</deckgo-social>
```

Without icon:

<p>
  <deckgo-social twitter="daviddalbusco"></deckgo-social>
</p>

```
<deckgo-social twitter="daviddalbusco">
</deckgo-social>
```

### Attributes

This component offers the following options which could be set using attributes:

| Attribute | Type   | Default | Description                                                                                         |
| --------- | ------ | ------- | --------------------------------------------------------------------------------------------------- |
| twitter   | string |         | Your Twitter username. It will be concatenated automatically with `https://twitter.com/`            |
| linkedin  | string |         | Your Linkedin username. It will be concatenated automatically with `https://www.linkedin.com/in/`   |
| medium    | string |         | Your Medium username. `username` will be replaced automatically from `https://username.medium.com/` |
| dev       | string |         | Your Dev username. It will be concatenated automatically with `https://dev.to/`                     |
| github    | string |         | Your Github username. It will be concatenated automatically with `https://github.com/`              |
| fullUrl   | string |         | In case you would like to provide the URI of your choice                                            |

### Examples

Without any icons:

```
<deckgo-social twitter="daviddalbusco">Twitter</deckgo-social>
<deckgo-social linkedin="david-dal-busco">Linkedin</deckgo-social>
<deckgo-social medium="david.dalbusco">Medium</deckgo-social>
<deckgo-social full-url="https://stackoverflow.com/users/5404186/peter-parker">Stackoverflow</deckgo-social>
```

With for example `ion-icon`:

```
<deckgo-social twitter="daviddalbusco">
  <ion-icon slot="icon" name="logo-twitter"></ion-icon>
  Twitter
</deckgo-social>

<deckgo-social github="fluster/deckdeckgo">
  <ion-icon slot="icon" name="logo-github"></ion-icon>
  DeckDeckGo on Github
</deckgo-social>
```

[deckdeckgo]: https://deckdeckgo.com
