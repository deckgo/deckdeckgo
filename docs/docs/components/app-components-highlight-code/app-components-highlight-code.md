# Highlight Code

The "Highlight Code" component is an extra component which let you highlight code easily.

To highlight your code, this component is using [Prism.js](https://prismjs.com) from [Lea Verou](http://lea.verou.me) and [James DiGioia](https://twitter.com/jamesdigioia).

Moreover, per default, your code will be displayed in form of a stylish "windowed" card as the amazing [carbon](https://carbon.now.sh), the tool to create and share beautiful images of your source code, would do.

## Table of contents

- [Showcase](#app-components-highlight-code-showcase)
- [Installation](#app-components-highlight-code-installation)
  - [Using DeckDeckGo Highlight Code from a CDN](#app-components-highlight-code-using-deckdeckgo-highlight-code-from-a-cdn)
  - [Install DeckDeckGo Highlight Code from NPM](#app-components-highlight-code-install-deckdeckgo-highlight-code-from-npm)
  - [Framework integration](#app-components-highlight-code-framework-integration)
- [Usage](#app-components-highlight-code-usage)
  - [Properties](#app-components-highlight-code-properties)
  - [Styling](#app-components-highlight-code-styling)
  - [Methods](#app-components-highlight-code-methods)
    - [Find the next anchor](#app-components-highlight-code-find-the-next-anchor)
    - [Zoom into code](#app-components-highlight-code-zoom-into-code)
    - [Load or reload the component](#app-components-highlight-code-load-or-reload-the-component)
  - [Events](#app-components-highlight-code-events)
  - [Examples](#app-components-highlight-code-examples)

## Showcase

```java
public static void main(String args[]) { 
  System.out.println("Hello World");
}
```

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using DeckDeckGo Highlight Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/highlight-code@latest/dist/deckdeckgo-highlight-code/deckdeckgo-highlight-code.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/highlight-code@latest/dist/deckdeckgo-highlight-code/deckdeckgo-highlight-code.js"></script>
```
### Install DeckDeckGo Highlight Code from NPM

Install [DeckDeckGo] - Highlight Code in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/highlight-code) using the following command:

```bash
npm install @deckdeckgo/highlight-code
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/highlight-code';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/highlight-code/dist/loader';
deckDeckGoElement(window);
```

## Usage

The `<deckgo-highlight-code/>` Web Component will highlight your code using [Prism.js](https://prismjs.com). You could inject a `<code/>` tag using slot or provide an URI to the file containing your code.

### Properties

The `<deckgo-highlight-code/>` expose the following properties:

| Property     | Attribute     | Description | Type      | Default               |
| ------------ | ------------- | ----------- | --------- | --------------------- |
| `src`        | `src`         | The web url to the source code you would like to showcase            | `string`  |            |
| `anchor`     | `anchor`      | The anchor identifier which will be use to find the next anchor to scroll too using `findNextAnchor()` | `string`  | `'// DeckDeckGo'`     |
| `anchorZoom` | `anchor-zoom` | The anchor identifier which will be use to find the next anchor to zoom inside your code using `findNextAnchor()`            | `string`  | `'// DeckDeckGoZoom'` |
| `hideAnchor` | `hide-anchor` | Set this attribute to `false` in case you would like to actually display the anchor value too            | `boolean` | `true`                |
| `language`   | `language`    |  Define the language to be used for the syntax highlighting. The list of [supported languages](https://prismjs.com/#languages-list) is defined by [Prism.js](https://prismjs.com/#languages-list)            | `string`  | `'javascript'`        |
| `highlightLines` | `highlight-lines` | If you wish to highlight some lines of your code. The lines number should be provided as number separated with coma and group separated with space. For example: "3,5 8,9 13,13 14,17" | `string`  |            |
| `lineNumbers`    | `line-numbers`    | Display the number of the lines of code | `boolean` | `false`               |
| `editable` | `editable` | In case you would like to set the code component as being editable. | `boolean` | `false`                |
| `carbon` | `carbon` | Present the code in a stylish "windowed" card. | `boolean` | `true`                | 

### Styling

The `<deckgo-highlight-code/>` could be styled using the following CSS4 variables:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-highlight-code-display | block | The display property of the code |
| --deckgo-highlight-code-color | inherit | The color of the displayed code |
| --deckgo-highlight-code-background | | The background of the displayed code |
| --deckgo-highlight-code-padding | 0 16px | The padding of the displayed code |
| --deckgo-highlight-code-border-radius |  | The border radius of the displayed code |
| --deckgo-highlight-code-margin-bottom | 16px 0 or 0 0 16px | Margin bottom of the code |
| --deckgo-highlight-code-zoom | 1 | If you wish to manually zoom the code |
| --deckgo-highlight-code-font-size |  | The size of the font for the code |
| --deckgo-highlight-code-font-family | monospace | The family of the font for the code |
| --deckgo-highlight-code-line-background | #3E4564 | The background of the lines you wish to highlight |
| --deckgo-highlight-code-line-numbers | #999999  | The color of the line numbers and divider |
| --deckgo-highlight-code-line-padding |  | A padding for each lines you wish to highlight |
| --deckgo-highlight-code-line-border-top |  | The border-top property of the lines you wish to highlight |
| --deckgo-highlight-code-direction | ltr | The direction of the displayed code |
| --deckgo-highlight-code-text-align | start | The text alignment of your code |
| --deckgo-highlight-code-token-comment | #6272a4 | Highlighted code tokens comment, prolog, doctype and cdata |
| --deckgo-highlight-code-token-punctuation | inherit | Highlighted code token punctuation |
| --deckgo-highlight-code-token-property | #bd93f9 | Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted |
| --deckgo-highlight-code-token-selector | #50fa7b | Highlighted code tokens selector, attr-name, string, char, builtin, inserted |
| --deckgo-highlight-code-token-operator | #ff79c6 | Highlighted code tokens operator, entity, url, string |
| --deckgo-highlight-code-token-atrule | #ff79c6 | Highlighted code tokens atrule, attr-value, keyword |
| --deckgo-highlight-code-token-function | #ffb86c | Highlighted code function, class-name |
| --deckgo-highlight-code-token-regex | #f1fa8c | Highlighted code tokens regex, important, variable |
| --deckgo-highlight-code-empty-text | "Click to add your code" | Place holder in case the `editable` is set to `true` |
| --deckgo-highlight-code-scroll | scroll | In case you would like to change the scroll property of the shadowed code block |
| --deckgo-highlight-code-container-width | | The attribute width of the code's container |
| --deckgo-highlight-code-container-height | | The attribute height of the code's container |
| --deckgo-highlight-code-container-display | | The attribute display of the code's container |
| --deckgo-highlight-code-container-justify-content | | The attribute justify-content of the code's container |
| --deckgo-highlight-code-container-flex-direction | | The attribute flex-direction of the code's container |
| --deckgo-highlight-code-container-align-items | | The attribute align-items of the code's container |

Furthermore the following styles apply if the code is displayed as a "windowed" card (`carbon` property equals to `true`):

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-highlight-code-carbon-display | block | The display property of the host container. |
| --deckgo-highlight-code-carbon-overflow | auto | The overflow property of the host container. |
| --deckgo-highlight-code-carbon-border |  | The border property of the host container. |
| --deckgo-highlight-code-carbon-border-radius | 4px | The border-radius property of the host container. |
| --deckgo-highlight-code-carbon-background | #282a36 | The background property of the host container. |
| --deckgo-highlight-code-carbon-color | white | The color property of the host container. |
| --deckgo-highlight-code-carbon-box-shadow | rgba(0, 0, 0, 0.55) 0 8px 16px | The box-shadow property of the host container. |
| --deckgo-highlight-code-carbon-margin | 16px 0 | The margin property of the host container. |
| --deckgo-highlight-code-carbon-header-padding | 16px | The padding property of the card header. |
| --deckgo-highlight-code-carbon-header-button-width | 12px | The width of a button of the card header. |
| --deckgo-highlight-code-carbon-header-button-heeght | 12px | The height of a button of the card header. |
| --deckgo-highlight-code-carbon-header-button-border-radius | 50% | The border-radius of a button of the card header. |
| --deckgo-highlight-code-carbon-header-button-margin | 0 6px 0 0 | The margin of a button of the card header. |
| --deckgo-highlight-code-carbon-header-button-red-background | #FF5F56 | The background of the first button of the card header. |
| --deckgo-highlight-code-carbon-header-button-red-border | 0.5px solid #E0443E | The border of the first button of the card header. |
| --deckgo-highlight-code-carbon-header-button-yellow-background | #FFBD2E | The background of the second button of the card header. |
| --deckgo-highlight-code-carbon-header-button-yellow-border | 0.5px solid #DEA123 | The border of the second button of the card header. |
| --deckgo-highlight-code-carbon-header-button-green-background | #27C93F | The background of the third button of the card header. |
| --deckgo-highlight-code-carbon-header-button-green-border | 0.5px solid #1AAB29 | The color of the third button of the card header. |

### Methods

The `<deckgo-highlight-code/>` expose the following methods:

#### Find the next anchor

```
findNextAnchor(enter: boolean) => Promise<boolean>
```

#### Zoom into code

```
zoomCode(zoom: boolean) => Promise<void>
```

#### Load or reload the component

```
load() => Promise<void>
```

### Events

The `<deckgo-highlight-code/>` will bubble the following events:

#### Code did change

Emitted when the code was edited (see attribute `editable`). Propagate the root component itself.

```
codeDidChange(HTMLElement);
```

### Examples

You could find the examples in the [src/index.html](https://github.com/deckgo/deckdeckgo/tree/master/webcomponents/highlight-code/src/index.html) of the project.

```
<deckgo-highlight-code src="https://domain.com/yourfile.js">
</deckgo-highlight-code>

<deckgo-highlight-code language="java" highlight-lines="2,2">
  <code slot="code">public static void main(String args[]) {
  
  System.out.println("Hello World");
}</code>
</deckgo-highlight-code>
```

[DeckDeckGo]: https://deckdeckgo.com