# Demo

Write and render math expressions.

This component is using [Katex](https://katex.org/) to renders these.

## Table of contents

- [Showcase](#app-components-math-showcase)
- [Installation](#app-components-math-installation)
  - [Using from a CDN](#app-components-math-from-a-cdn)
  - [Install from NPM](#app-components-math-from-npm)
  - [Framework integration](#app-components-math-framework-integration)
- [Usage](#app-components-math-usage)
  - [Slots](#app-components-math-slots)
  - [Attributes](#app-components-math-attributes)
  - [Events](#app-components-math-events)
  - [Theming](#app-components-math-theming)

## Showcase

<div style={{position: 'relative'}}>
  <deckgo-math>
    <code slot="math">{`% \\f is defined as f(#1) using the macro
      \\f{x} = \\int_{-\\infty}^\\infty
      \\hat \\f\\xi\\,e^{2 \\pi i \\xi x}
      \\,d\\xi`}</code>
  </deckgo-math>
</div>

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/math@latest/dist/deckdeckgo-math/deckdeckgo-math.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/math) using the following command:

```bash
npm install @deckdeckgo/math
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/math';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/math/dist/loader';
deckDeckGoElement();
```

## Usage

The "Math" Web Component could be integrated using the tag `<deckgo-math/>`.

```
<deckgo-math>
    <code slot="math">% \f is defined as f(#1) using the macro
        \f{x} = \int_{-\infty}^\infty
        \hat \f\xi\,e^{2 \pi i \xi x}
        \,d\xi</code>
</deckgo-math>
```

It either supports a single expression, as displayed above, or expressions within paragraphs.

```
<deckgo-math editable="true">
  <code slot="math">
    You can write math expression inside paragraph like this: $x + 1$

    Inline formulas can be written with \$ e.g: $c = \pm\sqrt{a^2 + b^2}$

    and displayed equations can be written using \$$ e.g: $$\sum_{i=1}^n 2^i$$
  </code>
</deckgo-math>
```

### Slots

The expressions have to be provided using the slot `math`.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute | Type    | Default                     | Description                                                                                                                                                                                                                                                                                        |
| --------- | ------- | --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| macros    | string  | &#123;"\\\\f":"f(#1)"&#123; | A collection of custom macros. Each macro is a property with a name like \name (written "\\name" in JavaScript) which maps to a string that describes the expansion of the macro, or a function that accepts an instance of MacroExpander as first argument and returns the expansion as a string. |
| editable  | boolean | `false`                     | To set the component has being editable (`contenteditable` will be applied on the `slot` on `click`)                                                                                                                                                                                               |

See the [Katex](https://katex.org/docs/options.html) documentation for more information.

### Events

The `<deckgo-math/>` component triggers the following event.

| Event           | Description                          | Type                       |
| --------------- | ------------------------------------ | -------------------------- |
| `mathDidChange` | Emit the host element when modified. | `CustomEvent<HTMLElement>` |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                           | Default                             | Note                                             |
| --------------------------------------- | ----------------------------------- | ------------------------------------------------ |
| --deckgo-math-color                     | inherit                             | Color                                            |
| --deckgo-math-background                |                                     | Background                                       |
| --deckgo-math-padding                   | 8px                                 | Padding                                          |
| --deckgo-math-border-radius             |                                     | Border radius                                    |
| --deckgo-math-margin                    | 0px                                 | Margin                                           |
| --deckgo-math-direction                 |                                     | Direction                                        |
| --deckgo-math-text-align                |                                     | Text alignment                                   |
| --deckgo-math-container-display         | block                               | Container display                                |
| --deckgo-math-container-justify-content |                                     | Container justify-content attribute              |
| --deckgo-math-container-flex-direction  |                                     | Container flex-direction attribute               |
| --deckgo-math-container-align-items     |                                     | Container align-items attribute                  |
| --deckgo-math-scroll                    | scroll                              | Scroll property of the expression(s)             |
| --deckgo-math-font-size                 |                                     | Font size property of the expression(s)          |
| --deckgo-math-min-height                | 23px                                | Minimal height property of the expression(s)     |
| --deckgo-math-display                   | block                               | Display property of the expression(s)            |
| --deckgo-math-code-empty-text           | "Click to add your math expression" | Place holder in case `editable` is set to `true` |

[deckdeckgo]: https://deckdeckgo.com
