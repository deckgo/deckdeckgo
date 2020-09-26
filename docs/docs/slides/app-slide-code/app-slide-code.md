# Slide: Code

The "Code" slide is a the slide to use if you would like to showcase code during your talk.

## Table of contents

- [Layout](#app-slide-code-layout)
- [Installation](#app-slide-code-installation)
  - [From a CDN](#app-slide-code-from-a-cdn)
  - [From NPM](#app-slide-code-from-npm)
  - [Framework integration](#app-slide-code-framework-integration)
- [Usage](#app-slide-code-usage)
  - [Usage with file URI](#app-slide-code-usage-with-file-uri)
  - [Usage with slotted element](#app-slide-code-usage-with-slotted-element)
  - [Slots](#app-slide-code-slots)
- [Code components](#app-slide-code-code-components)
- [Installation](#app-slide-code-installation)
- [Attributes](#app-slide-code-attributes)
  - [Example with file URI](#app-slide-code-example-with-file-uri)
  - [Example with slotted element](#app-slide-code-example-with-slotted-element)
- [Theming](#app-slide-code-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-code src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/code/src/components/slide/deckdeckgo-slide-code.tsx">
          <h1 slot="title">slot="title"</h1>
        </deckgo-slide-code>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-code@latest/dist/deckdeckgo-slide-code/deckdeckgo-slide-code.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/slide-code@latest/dist/deckdeckgo-slide-code/deckdeckgo-slide-code.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-code) run the following command:

```bash
npm install @deckdeckgo/slide-code
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-code';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-code/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Code" slide's Web Component could be integrated using the tag `<deckgo-slide-code/>`.

You could provide a file URI to the code you want to display or provide it with a slotted element.

### Usage with file URI

```
<deckgo-deck>
  <deckgo-slide-code src="https://domain.com/path-to-my-code.extension">
    <h1 slot="title">My code</h1>
  </deckgo-slide-code>
</deckgo-deck>
```

### Usage with slotted element

```
<deckgo-deck>
  <deckgo-slide-code language="java">
      <h1 slot="title">Manual code</h1>
      <code slot="code">
  interface DeckDeckGoDemo {
    boolean helloWorld();
  }
  </deckgo-slide-code>
</deckgo-deck>
```

### Slots

The slots `title` and `code` are optional.

## Code components

The slide "Code" relies on the code component `<deckgo-highlight-code/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Installation

The [DeckDeckGo] - Hightlight code component is provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] code to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/@deckdeckgo/highlight-code) as described in its [installation guide](https://docs.deckdeckgo.com/components/code#app-components-highlight-code-getting-started).

## Attributes

At least `src` or the `slot` code should be provided in order to render code in this template. It offers the same attributes as the [DeckDeckGo] code Web Component, see its [documentation](https://docs.deckdeckgo.com/components/code) for the details and the following other attributes:

| Attribute         | Type    | Default | Description                                                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

### Example with file URI

```
<deckgo-deck>
  <deckgo-slide-code hide-anchor="false" src="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
    <h1 slot="title">Code</h1>
  </deckgo-slide-code>
</deckgo-deck>
```

### Example with slotted element

```
<deckgo-deck>
  <deckgo-slide-code language="java">
      <h1 slot="title">Manual code</h1>
      <code slot="code">interface NumericTest {
    boolean computeTest(int n);
  }

  public static void main(String args[]) {
    NumericTest isEven = (n) -> (n % 2) == 0;
    NumericTest isNegative = (n) -> (n < 0);

    // Output: false
    System.out.println(isEven.computeTest(5));

    // Output: true
    System.out.println(isNegative.computeTest(-5));
  }</code>
  </deckgo-slide-code>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable          | Default | Note                            |
| ---------------------- | ------- | ------------------------------- |
| --background           |         |                                 |
| --color                |         |                                 |
| --slide-padding-top    | 16px    | Padding top of the all slide    |
| --slide-padding-end    | 32px    | Padding right of the all slide  |
| --slide-padding-bottom | 16px    | Padding bottom of the all slide |
| --slide-padding-start  | 32px    | Padding left of the all slide   |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] - Highlight code Web Component, see its [documentation](https://docs.deckdeckgo.com/components/code) for the details.

[deckdeckgo]: https://deckdeckgo.com
