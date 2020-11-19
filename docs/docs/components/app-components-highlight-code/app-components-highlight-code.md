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
  - [Fonts](#app-components-highlight-code-fonts)
  - [Styling](#app-components-highlight-code-styling)
  - [Methods](#app-components-highlight-code-methods)
    - [Find the next anchor](#app-components-highlight-code-find-the-next-anchor)
    - [Zoom into code](#app-components-highlight-code-zoom-into-code)
    - [Load or reload the component](#app-components-highlight-code-load-or-reload-the-component)
  - [Events](#app-components-highlight-code-events)
  - [Examples](#app-components-highlight-code-examples)

## Showcase

Carbon terminal card:

<div>
    <deckgo-highlight-code theme={this.theme}>
    <code slot="code">console.log('Hello World');</code>
    </deckgo-highlight-code>
</div>

<div class="ion-margin-bottom">
    <small>Theme:&nbsp;</small>
    <select style={{color: 'black'}}
        ref={(el) => (this.selectTheme = el as HTMLSelectElement)}
        onChange={() => {
          this.theme = this.selectTheme.value as DeckdeckgoHighlightCodeCarbonTheme;
        }}>
        {
            Object.keys(DeckdeckgoHighlightCodeCarbonTheme).map((key: DeckdeckgoHighlightCodeCarbonTheme) => {
              return <option selected={DeckdeckgoHighlightCodeCarbonTheme[key] === DeckdeckgoHighlightCodeCarbonTheme.DRACULA} value={DeckdeckgoHighlightCodeCarbonTheme[key]}>{DeckdeckgoHighlightCodeCarbonTheme[key].replace(/^\w/, (c) => c.toUpperCase())}</option>
            })
        }
    </select>
</div>

<div class="ion-padding-top">Ubuntu terminal card:</div>

<div>
    <deckgo-highlight-code terminal={DeckdeckgoHighlightCodeTerminal.UBUNTU}>
    <code slot="code">console.log('Hello World');</code>
    <span slot="user">david@ubuntu:~</span>
    </deckgo-highlight-code>
</div>

<div class="ion-padding-top">No terminal:</div>

<div>
    <deckgo-highlight-code terminal={DeckdeckgoHighlightCodeTerminal.NONE} style={{'--deckgo-highlight-code-padding': '0'}}>
    <code slot="code">console.log('Hello World');</code>
    </deckgo-highlight-code>
</div>

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using DeckDeckGo Highlight Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/highlight-code@latest/dist/deckdeckgo-highlight-code/deckdeckgo-highlight-code.esm.js"></script>
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
deckDeckGoElement();
```

## Usage

The `<deckgo-highlight-code/>` Web Component will highlight your code using [Prism.js](https://prismjs.com).

You could inject a `<code/>` tag using slot or provide an URI to the file containing your code.

If you are displaying your code in an Ubuntu terminal, you could also displays a text in the toolbar (header) using the slot `user`.

### Properties

The `<deckgo-highlight-code/>` expose the following properties:

| Property         | Attribute         | Description                                                                                                                                                                                      | Type                                                                                                                                                                                                                                                                                                                                                                          | Default               |
| ---------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------- |
| `src`            | `src`             | The web url to the source code you would like to showcase                                                                                                                                        | `string`                                                                                                                                                                                                                                                                                                                                                                      |                       |
| `anchor`         | `anchor`          | The anchor identifier which will be use to find the next anchor to scroll too using `findNextAnchor()`                                                                                           | `string`                                                                                                                                                                                                                                                                                                                                                                      | `'// DeckDeckGo'`     |
| `anchorZoom`     | `anchor-zoom`     | The anchor identifier which will be use to find the next anchor to zoom inside your code using `findNextAnchor()`                                                                                | `string`                                                                                                                                                                                                                                                                                                                                                                      | `'// DeckDeckGoZoom'` |
| `hideAnchor`     | `hide-anchor`     | Set this attribute to `false` in case you would like to actually display the anchor value too                                                                                                    | `boolean`                                                                                                                                                                                                                                                                                                                                                                     | `true`                |
| `language`       | `language`        | Define the language to be used for the syntax highlighting. The list of [supported languages](https://prismjs.com/#languages-list) is defined by [Prism.js](https://prismjs.com/#languages-list) | `string`                                                                                                                                                                                                                                                                                                                                                                      | `'javascript'`        |
| `highlightLines` | `highlight-lines` | If you wish to highlight some lines of your code. The lines number should be provided as number separated with coma and group separated with space. For example: "3,5 8,9 13,13 14,17"           | `string`                                                                                                                                                                                                                                                                                                                                                                      |                       |
| `lineNumbers`    | `line-numbers`    | Display the number of the lines of code                                                                                                                                                          | `boolean`                                                                                                                                                                                                                                                                                                                                                                     | `false`               |
| `editable`       | `editable`        | In case you would like to set the code component as being editable.                                                                                                                              | `boolean`                                                                                                                                                                                                                                                                                                                                                                     | `false`               |
| `terminal`       | `terminal`        | Present the code in a stylish "windowed" card.                                                                                                                                                   | `carbon`, `ubuntu` or `none`                                                                                                                                                                                                                                                                                                                                                  | `carbon`              |
| `theme`          | `theme`           | The theme of the selected `terminal` (applied only in case of `carbon`)                                                                                                                          | `3024-night`, `a11y-dark`, `blackboard`, `base16-dark`, `base16-light`, `cobalt`, `dracula`, `duotone`, `hopscotch`, `lucario`, `material`, `monokai`, `night-owl`, `nord`, `oceanic-next`, `one-light`, `one-dark`, `panda`, `paraiso`, `seti`, `shades-of-purple`, `solarized-dark`, `solarized-light`, `synthwave`, `twilight`, `verminal`, `vscode`, `yeti` and `zenburn` | `dracula`             |

### Fonts

Per default, the font `monospace` is used to display the code for the terminal `carbon` or `none`. You can overwrite this option using the following CSS variables.

If you display your code in an `ubuntu` terminal, the related `Ubuntu` fonts are going to be fetched and injected in your page.

### Styling

The `<deckgo-highlight-code/>` could be styled using the following CSS4 variables:

| CSS4 variable                                     | Default                  | Note                                                                              |
| ------------------------------------------------- | ------------------------ | --------------------------------------------------------------------------------- |
| --deckgo-highlight-code-display                   | block                    | The display property of the code                                                  |
| --deckgo-highlight-code-color                     | inherit                  | The color of the displayed code                                                   |
| --deckgo-highlight-code-background                |                          | The background of the displayed code                                              |
| --deckgo-highlight-code-padding                   | 0 16px                   | The padding of the displayed code                                                 |
| --deckgo-highlight-code-border-radius             |                          | The border radius of the displayed code                                           |
| --deckgo-highlight-code-margin-bottom             | 16px 0 or 0 0 16px       | Margin bottom of the code                                                         |
| --deckgo-highlight-code-zoom                      | 1                        | If you wish to manually zoom the code                                             |
| --deckgo-highlight-code-white-space               | pre-wrap                 | The attribute white-space of the displayed                                        |
| code                                              |
| --deckgo-highlight-code-font-size                 |                          | The size of the font for the code                                                 |
| --deckgo-highlight-code-font-family               | monospace                | The family of the font for the code                                               |
| --deckgo-highlight-code-line-background           | #3E4564                  | The background of the lines you wish to highlight                                 |
| --deckgo-highlight-code-line-numbers              | #999999                  | The color of the line numbers and divider                                         |
| --deckgo-highlight-code-line-padding              |                          | A padding for each lines you wish to highlight                                    |
| --deckgo-highlight-code-line-border-top           |                          | The border-top property of the lines you wish to highlight                        |
| --deckgo-highlight-code-direction                 | ltr                      | The direction of the displayed code                                               |
| --deckgo-highlight-code-text-align                | start                    | The text alignment of your code                                                   |
| --deckgo-highlight-code-token-comment             | #6272a4                  | Highlighted code tokens comment, prolog, doctype and cdata                        |
| --deckgo-highlight-code-token-punctuation         | inherit                  | Highlighted code token punctuation                                                |
| --deckgo-highlight-code-token-property            | #bd93f9                  | Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted |
| --deckgo-highlight-code-token-selector            | #50fa7b                  | Highlighted code tokens selector, attr-name, string, char, builtin, inserted      |
| --deckgo-highlight-code-token-operator            | #ff79c6                  | Highlighted code tokens operator, entity, url, string                             |
| --deckgo-highlight-code-token-atrule              | #ff79c6                  | Highlighted code tokens atrule, attr-value, keyword                               |
| --deckgo-highlight-code-token-function            | #ffb86c                  | Highlighted code function, class-name                                             |
| --deckgo-highlight-code-token-regex               | #f1fa8c                  | Highlighted code tokens regex, important, variable                                |
| --deckgo-highlight-code-empty-text                | "Click to add your code" | Place holder in case the `editable` is set to `true`                              |
| --deckgo-highlight-code-scroll                    | auto                     | In case you would like to change the scroll property of the shadowed code block   |
| --deckgo-highlight-code-container-width           |                          | The attribute width of the code's container                                       |
| --deckgo-highlight-code-container-height          |                          | The attribute height of the code's container                                      |
| --deckgo-highlight-code-container-display         |                          | The attribute display of the code's container                                     |
| --deckgo-highlight-code-container-justify-content |                          | The attribute justify-content of the code's container                             |
| --deckgo-highlight-code-container-flex-direction  |                          | The attribute flex-direction of the code's container                              |
| --deckgo-highlight-code-container-align-items     |                          | The attribute align-items of the code's container                                 |

#### Carbon

Furthermore the following styles apply if the code is displayed as a "carbon" terminal card (`terminal` property equals to `carbon`).

| CSS4 variable                                                  | Default                        | Note                                                    |
| -------------------------------------------------------------- | ------------------------------ | ------------------------------------------------------- |
| --deckgo-highlight-code-carbon-display                         | block                          | The display property of the host container.             |
| --deckgo-highlight-code-carbon-toolbar-display                 | block                          | The display property of the toolbar container.          |
| --deckgo-highlight-code-carbon-overflow                        | auto                           | The overflow property of the host container.            |
| --deckgo-highlight-code-carbon-border                          |                                | The border property of the host container.              |
| --deckgo-highlight-code-carbon-border-radius                   | 4px                            | The border-radius property of the host container.       |
| --deckgo-highlight-code-carbon-background                      | #282a36                        | The background property of the host container.          |
| --deckgo-highlight-code-carbon-color                           | white                          | The color property of the host container.               |
| --deckgo-highlight-code-carbon-box-shadow                      | rgba(0, 0, 0, 0.55) 0 8px 16px | The box-shadow property of the host container.          |
| --deckgo-highlight-code-carbon-margin                          | 16px 0                         | The margin property of the host container.              |
| --deckgo-highlight-code-carbon-header-padding                  | 16px                           | The padding property of the card header.                |
| --deckgo-highlight-code-carbon-header-margin                   | 0                              | The margin property of the card header.                 |
| --deckgo-highlight-code-carbon-header-button-width             | 12px                           | The width of a button of the card header.               |
| --deckgo-highlight-code-carbon-header-button-height            | 12px                           | The height of a button of the card header.              |
| --deckgo-highlight-code-carbon-header-button-border-radius     | 50%                            | The border-radius of a button of the card header.       |
| --deckgo-highlight-code-carbon-header-button-margin            | 0 6px 0 0                      | The margin of a button of the card header.              |
| --deckgo-highlight-code-carbon-header-button-red-background    | #FF5F56                        | The background of the first button of the card header.  |
| --deckgo-highlight-code-carbon-header-button-red-border        | 0.5px solid #E0443E            | The border of the first button of the card header.      |
| --deckgo-highlight-code-carbon-header-button-yellow-background | #FFBD2E                        | The background of the second button of the card header. |
| --deckgo-highlight-code-carbon-header-button-yellow-border     | 0.5px solid #DEA123            | The border of the second button of the card header.     |
| --deckgo-highlight-code-carbon-header-button-green-background  | #27C93F                        | The background of the third button of the card header.  |
| --deckgo-highlight-code-carbon-header-button-green-border      | 0.5px solid #1AAB29            | The color of the third button of the card header.       |

#### Ubuntu

If the code is displayed as an "ubuntu" terminal card (`terminal` property equals to `ubuntu`) the following styles could be applied.

| CSS4 variable                                                    | Default                                          | Note                                               |
| ---------------------------------------------------------------- | ------------------------------------------------ | -------------------------------------------------- |
| --deckgo-highlight-code-ubuntu-display                           | block                                            | The display property of the host container.        |
| --deckgo-highlight-code-ubuntu-overflow                          | auto                                             | The overflow property of the host container.       |
| --deckgo-highlight-code-ubuntu-border                            |                                                  | The border property of the host container.         |
| --deckgo-highlight-code-ubuntu-border-radius                     | 6px 6px 0 0                                      | The border-radius property of the host container.  |
| --deckgo-highlight-code-ubuntu-background                        | rgba(56, 4, 40, 0.9)                             | The background property of the host container.     |
| --deckgo-highlight-code-ubuntu-color                             | #ddd                                             | The color property of the host container.          |
| --deckgo-highlight-code-ubuntu-box-shadow                        | 2px 4px 10px rgba(0, 0, 0, 0.5)                  | The box-shadow property of the host container.     |
| --deckgo-highlight-code-ubuntu-margin                            | 16px 0                                           | The margin property of the host container.         |
| --deckgo-highlight-code-ubuntu-header-padding                    | 0 8px                                            | The padding property of the card header.           |
| --deckgo-highlight-code-ubuntu-header-height                     | 25px                                             | The height property of the card header.            |
| --deckgo-highlight-code-ubuntu-header-background                 | linear-gradient(#504b45 0%, #3c3b37 100%)        | The background property of the card header.        |
| --deckgo-highlight-code-ubuntu-header-font-family                | Ubuntu                                           | The font-family property of the card header.       |
| --deckgo-highlight-code-ubuntu-header-button-width               | 12px                                             | The width of a button of the card header.          |
| --deckgo-highlight-code-ubuntu-header-button-height              | 12px                                             | The height of a button of the card header.         |
| --deckgo-highlight-code-ubuntu-header-button-border-radius       | 50%                                              | The border-radius of a button of the card header.  |
| --deckgo-highlight-code-ubuntu-header-button-margin              | 0 6px 0 0                                        | The margin of a button of the card header.         |
| --deckgo-highlight-code-ubuntu-header-button-font-size           | 7px                                              | The font-size of a button of the card header.      |
| --deckgo-highlight-code-ubuntu-header-button-color               | black                                            | The color of a button of the card header.          |
| --deckgo-highlight-code-ubuntu-header-button-text-shadow         | 0px 1px 0px rgba(255, 255, 255, 0.2)             | The text-shadow of a button of the card header.    |
| --deckgo-highlight-code-ubuntu-header-button-box-shadow          | 0px 0px 1px 0px #41403a, 0px 1px 1px 0px #474642 | The box-shadow of a button of the card header.     |
| --deckgo-highlight-code-ubuntu-header-button-close-background    | linear-gradient(#f37458 0%, #de4c12 100%)        | The close button background of the card header.    |
| --deckgo-highlight-code-ubuntu-header-button-close-border        |                                                  | The close button border of the card header.        |
| --deckgo-highlight-code-ubuntu-header-button-minimize-background | linear-gradient(#7d7871 0%, #595953 100%)        | The minimize button background of the card header. |
| --deckgo-highlight-code-ubuntu-header-button-minimize-border     |                                                  | The minimize button border of the card header.     |
| --deckgo-highlight-code-ubuntu-header-button-maximize-background | linear-gradient(#7d7871 0%, #595953 100%)        | The maximize button background of the card header. |
| --deckgo-highlight-code-ubuntu-header-button-maximize-border     |                                                  | The maximize button border of the card header.     |
| --deckgo-highlight-code-ubuntu-header-user-color                 | #d5d0ce                                          | The user's color of the card header.               |
| --deckgo-highlight-code-ubuntu-header-user-font-size             | 12px                                             | The user's font-size of the card header.           |
| --deckgo-highlight-code-ubuntu-header-user-line-height           | 14px                                             | The user's line-height of the card header.         |
| --deckgo-highlight-code-ubuntu-header-user-margin                | 0 0 1px 4px                                      | The user's margin of the card header.              |

### Methods

The `<deckgo-highlight-code/>` exposes the following methods:

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

```
<deckgo-highlight-code terminal="ubuntu">
    <code slot="code">console.log('Hello World');</code>
    <span slot="user">david@ubuntu:~</span>
</deckgo-highlight-code>
```

[deckdeckgo]: https://deckdeckgo.com
