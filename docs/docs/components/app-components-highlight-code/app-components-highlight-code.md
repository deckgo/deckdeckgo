# Highlight Code

The "Highlight Code" component is an extra component which let you highlight code easily.

To highlight your code, this component is using [Prism.js](https://prismjs.com) from [James DiGioia](https://twitter.com/jamesdigioia).

## Table of contents

- [Showcase](#app-components-highlight-code-showcase)
- [Getting started](#app-components-highlight-code-getting-started)
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
  - [Examples](#app-components-highlight-code-examples)

## Showcase

```java
public static void main(String args[]) { 
  System.out.println("Hello World");
}
```

## Getting started

To create easily your PWA presentation and to enjoy all the options, I suggest you to create your slides using the CLI as described in the [Getting started chapter](/docs/introduction).

Doing so you will use the [DeckDeckGo] starter kit which already includes this Web Component.

However, if you are looking to use this Web Component as a standalone component, to add a code highlighter feature to your web applications, it could be use directly in your project from a CDN, using a simple script include, or could be installed from [npm](https://www.npmjs.com/package/deckdeckgo-highlight-code).

### Using DeckDeckGo Highlight Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script src="https://unpkg.com/deckdeckgo-highlight-code@latest/dist/deckdeckgo-highlight-code.js"></script>
```
### Install DeckDeckGo Highlight Code from NPM

Install [DeckDeckGo] - Highlight Code in your project from [npm](https://www.npmjs.com/package/deckdeckgo-highlight-code) using the following command:

```bash
npm install deckdeckgo-highlight-code
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

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

### Styling

The `<deckgo-highlight-code/>` could be styled using the following CSS4 variables:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-highlight-code-color | inherit | The color of the displayed code |
| --deckgo-highlight-code-background |  | The background of the displayed code |
| --deckgo-highlight-code-padding |  | The padding of the displayed code |
| --deckgo-highlight-code-border-radius |  | The border radius of the displayed code |
| --deckgo-highlight-code-margin-bottom | 64px | Margin bottom of the code scroller |
| --deckgo-highlight-code-zoom | 1 | If you wish to manually zoom the code |
| --deckgo-highlight-code-font-size |  | The size of the font for the code |
| --deckgo-highlight-code-font-family | monospace | The family of the font for the code |
| --deckgo-highlight-code-line-background |  | The background of the lines you wish to highlight |
| --deckgo-highlight-code-line-padding |  | A padding for each lines you wish to highlight |
| --deckgo-highlight-code-line-border-top |  | The border-top property of the lines you wish to highlight |
| --deckgo-highlight-code-direction | ltr | The direction of the displayed code |
| --deckgo-highlight-code-text-align | start | The text alignment of your code |
| --deckgo-highlight-code-token-comment |  | Highlighted code tokens comment, prolog, doctype and cdata |
| --deckgo-highlight-code-token-punctuation |  | Highlighted code token punctuation |
| --deckgo-highlight-code-token-property |  | Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted |
| --deckgo-highlight-code-token-selector |  | Highlighted code tokens selector, attr-name, string, char, builtin, inserted |
| --deckgo-highlight-code-token-operator |  | Highlighted code tokens operator, entity, url, string |
| --deckgo-highlight-code-token-atrule |  | Highlighted code tokens atrule, attr-value, keyword |
| --deckgo-highlight-code-token-function |  | Highlighted code function, class-name |
| --deckgo-highlight-code-token-regex |  | Highlighted code tokens regex, important, variable |

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

### Examples

You could find the examples in the [src/index.html](https://github.com/deckgo/deckdeckgo-qrcode/blob/master/src/index.html) of the project.

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