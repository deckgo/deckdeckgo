# deckgo-math

## Installation

This component can be added to your web application with following methods.

> If you are using our Starter Kit, no need to worry about this, this component is included, therefore you could skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

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

<!-- Auto Generated Below -->


## Properties

| Property   | Attribute  | Description                                                                                                                                                                                                                                                                                       | Type      | Default               |
| ---------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------- | --------------------- |
| `editable` | `editable` | To set the component has being editable (contenteditable will be applied on the slot on click)                                                                                                                                                                                                    | `boolean` | `false`               |
| `macros`   | `macros`   | A collection of custom macros. Each macro is a property with a name like \name (written "\name" in JavaScript) which maps to a string that describes the expansion of the macro, or a function that accepts an instance of MacroExpander as first argument and returns the expansion as a string. | `string`  | ``{"\\\\f":"f(#1)"}`` |


## Events

| Event           | Description                         | Type                       |
| --------------- | ----------------------------------- | -------------------------- |
| `mathDidChange` | Emit the host element when modified | `CustomEvent<HTMLElement>` |


## Slots

| Slot     | Description                   |
| -------- | ----------------------------- |
| `"math"` | The math expression to render |


## CSS Custom Properties

| Name                                      | Description                                                                             |
| ----------------------------------------- | --------------------------------------------------------------------------------------- |
| `--deckgo-math-background`                | Background                                                                              |
| `--deckgo-math-border-radius`             | Border radius                                                                           |
| `--deckgo-math-color`                     | Color @default inherit                                                                  |
| `--deckgo-math-container-align-items`     | Container align-items attribute                                                         |
| `--deckgo-math-container-display`         | Container display @default bloack                                                       |
| `--deckgo-math-container-flex-direction`  | Container flex-direction attribute                                                      |
| `--deckgo-math-container-justify-content` | Container justify-content attribute                                                     |
| `--deckgo-math-direction`                 | Direction                                                                               |
| `--deckgo-math-display`                   | Display property of the expression(s) @default block                                    |
| `--deckgo-math-empty-text`                | Place holder in case editable is set to true @default Click to add your math expression |
| `--deckgo-math-font-size`                 | Font size property of the expression(s)                                                 |
| `--deckgo-math-margin`                    | Margin @default 0px                                                                     |
| `--deckgo-math-min-height`                | Minimal height property of the expression(s) @default 23px                              |
| `--deckgo-math-padding`                   | Padding @default 8px                                                                    |
| `--deckgo-math-scroll`                    | Scroll property of the expression(s) @default scroll                                    |
| `--deckgo-math-text-align`                | Text alignment                                                                          |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
