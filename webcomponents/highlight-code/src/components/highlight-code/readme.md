# deckgo-highlight-code

The "Highlight Code" component is an extra component which let you highlight code easily.

To highlight your code, this component is using [Prism.js](https://prismjs.com) from [Lea Verou](http://lea.verou.me) and [James DiGioia](https://twitter.com/jamesdigioia).

Moreover, per default, your code will be displayed in form of a stylish "windowed" card as the amazing [carbon](https://carbon.now.sh), the tool to create and share beautiful images of your source code, would do.

## Installation

This component can be added to your web application with following methods.

> If you are using our developer kit to create a presention, this component is already included

### Using DeckDeckGo Highlight Code from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo](https://deckdeckgo.com) Code from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/highlight-code@latest/dist/deckdeckgo-highlight-code/deckdeckgo-highlight-code.esm.js"></script>
```

### Install DeckDeckGo Highlight Code from NPM

Install [DeckDeckGo](https://deckdeckgo.com) - Highlight Code in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/highlight-code) using the following command:

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

<!-- Auto Generated Below -->


## Properties

| Property         | Attribute         | Description                                                                                                                                                                                                                                                               | Type                                                                                                                       | Default                                      |
| ---------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------- |
| `editable`       | `editable`        | In case you would like to set the code component as being editable                                                                                                                                                                                                        | `boolean`                                                                                                                  | `false`                                      |
| `highlightLines` | `highlight-lines` | If you wish to highlight some lines of your code. The lines number should be provided as a number (one line) or numbers separated with coma or dash (many lines), group separated with space. For example: 1 3,5 8 14-17 which highlight lines  1, 3 to 5, 8 and 14 to 17 | `string`                                                                                                                   | `undefined`                                  |
| `language`       | `language`        | Define the language to be used for the syntax highlighting. The list of supported languages is defined by Prism.js                                                                                                                                                        | `string`                                                                                                                   | `'javascript'`                               |
| `lineNumbers`    | `line-numbers`    | Display the number of the lines of code                                                                                                                                                                                                                                   | `boolean`                                                                                                                  | `false`                                      |
| `terminal`       | `terminal`        | Present the code in a stylish "windowed" card                                                                                                                                                                                                                             | `DeckdeckgoHighlightCodeTerminal.CARBON \| DeckdeckgoHighlightCodeTerminal.NONE \| DeckdeckgoHighlightCodeTerminal.UBUNTU` | `DeckdeckgoHighlightCodeTerminal.CARBON`     |
| `theme`          | `theme`           | The theme of the selected terminal (applied only in case of carbon)                                                                                                                                                                                                       | `DeckdeckgoHighlightCodeCarbonTheme`                                                                                       | `DeckdeckgoHighlightCodeCarbonTheme.DRACULA` |


## Events

| Event                 | Description                                                                                    | Type                       |
| --------------------- | ---------------------------------------------------------------------------------------------- | -------------------------- |
| `codeDidChange`       | Emitted when the code was edited (see attribute editable). Propagate the root component itself | `CustomEvent<HTMLElement>` |
| `prismLanguageLoaded` | Emitted when a language is fetched and loaded                                                  | `CustomEvent<string>`      |


## Methods

### `load() => Promise<void>`

Load or reload the component

#### Returns

Type: `Promise<void>`




## Slots

| Slot     | Description                         |
| -------- | ----------------------------------- |
| `"code"` | A `<code/>` element to highlight    |
| `"user"` | A user name for the Ubuntu terminal |


## CSS Custom Properties

| Name                                                               | Description                                                                                                                                      |
| ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `--deckgo-editable-cursor`                                         | The mouse cursor displayed when hovering the editable element @default text                                                                      |
| `--deckgo-highlight-code-border-radius`                            | The border radius of the displayed code                                                                                                          |
| `--deckgo-highlight-code-carbon-background`                        | The background property of the host container @default #282a36                                                                                   |
| `--deckgo-highlight-code-carbon-border`                            | The border property of the host container                                                                                                        |
| `--deckgo-highlight-code-carbon-border-radius`                     | The border-radius property of the host container @default 4px                                                                                    |
| `--deckgo-highlight-code-carbon-box-shadow`                        | The box-shadow property of the host container @default rgba(0, 0, 0, 0.55) 0 8px 16px)                                                           |
| `--deckgo-highlight-code-carbon-color`                             | The color property of the host container @default white                                                                                          |
| `--deckgo-highlight-code-carbon-display`                           | The display property of the host container @default block                                                                                        |
| `--deckgo-highlight-code-carbon-header-button-border-radius`       | The border-radius of a button of the card header @default 50%                                                                                    |
| `--deckgo-highlight-code-carbon-header-button-green-background`    | The background of the third button of the card header @default #27c93f                                                                           |
| `--deckgo-highlight-code-carbon-header-button-green-border`        | The color of the third button of the card header @default 0.5px solid #1aab29                                                                    |
| `--deckgo-highlight-code-carbon-header-button-height`              | The height of a button of the card header @default 12px                                                                                          |
| `--deckgo-highlight-code-carbon-header-button-margin`              | The margin of a button of the card header @default 8px 6px 8px 0                                                                                 |
| `--deckgo-highlight-code-carbon-header-button-red-background`      | The background of the first button of the card header @default #ff5f56                                                                           |
| `--deckgo-highlight-code-carbon-header-button-red-border`          | The border of the first button of the card header @default 0.5px solid #e0443e                                                                   |
| `--deckgo-highlight-code-carbon-header-button-width`               | The width of a button of the card header @default 12px                                                                                           |
| `--deckgo-highlight-code-carbon-header-button-yellow-background`   | The background of the second button of the card header @default #ffbd2e                                                                          |
| `--deckgo-highlight-code-carbon-header-button-yellow-border`       | The border of the second button of the card header @default 0.5px solid #dea123                                                                  |
| `--deckgo-highlight-code-carbon-header-margin`                     | The margin property of the card header @default 0                                                                                                |
| `--deckgo-highlight-code-carbon-header-padding`                    | The padding property of the card header. @default 8px 16px                                                                                       |
| `--deckgo-highlight-code-carbon-margin`                            | The margin property of the host container @default 16px 0                                                                                        |
| `--deckgo-highlight-code-carbon-overflow`                          | The overflow property of the host container. @default auto                                                                                       |
| `--deckgo-highlight-code-carbon-toolbar-display`                   | The display property of the toolbar container @default bloack                                                                                    |
| `--deckgo-highlight-code-container-align-items`                    | The attribute align-items of the code's container                                                                                                |
| `--deckgo-highlight-code-container-display`                        | The attribute display of the code's container @default bloack                                                                                    |
| `--deckgo-highlight-code-container-flex-direction`                 | The attribute flex-direction of the code's container                                                                                             |
| `--deckgo-highlight-code-container-height`                         | The attribute height of the code's container                                                                                                     |
| `--deckgo-highlight-code-container-justify-content`                | The attribute justify-content of the code's container                                                                                            |
| `--deckgo-highlight-code-container-width`                          | The attribute width of the code's container                                                                                                      |
| `--deckgo-highlight-code-direction`                                | The direction of the displayed code @default ltr                                                                                                 |
| `--deckgo-highlight-code-display`                                  | The display property of the code @default block                                                                                                  |
| `--deckgo-highlight-code-empty-text`                               | Place holder in case the editable is set to true @default Click to add your code                                                                 |
| `--deckgo-highlight-code-font-family`                              | The family of the font for the code @default monospace                                                                                           |
| `--deckgo-highlight-code-font-size`                                | The size of the font for the code                                                                                                                |
| `--deckgo-highlight-code-line-background`                          | The background of the lines you wish to highlight                                                                                                |
| `--deckgo-highlight-code-line-border-bottom`                       | The border-bottom property of the lines you wish to highlight                                                                                    |
| `--deckgo-highlight-code-line-border-top`                          | The border-top property of the lines you wish to highlight                                                                                       |
| `--deckgo-highlight-code-line-color`                               | The color of the lines you wish to highlight                                                                                                     |
| `--deckgo-highlight-code-line-font-weight`                         | The font-weight of the lines you wish to highlight                                                                                               |
| `--deckgo-highlight-code-line-height`                              | The line height of the font for the code                                                                                                         |
| `--deckgo-highlight-code-line-numbers-background`                  | The background property of the line numbers                                                                                                      |
| `--deckgo-highlight-code-line-numbers-border-right`                | The border right property of the line numbers @default 1px solid rgba(var(--deckgo-highlight-code-token-comment-rgb, 98, 114, 164), 0.32)        |
| `--deckgo-highlight-code-line-numbers-color`                       | The color property of the line numbers @default var(--deckgo-highlight-code-token-comment, #6272a4)                                              |
| `--deckgo-highlight-code-margin`                                   | The margin property of the code container @default 0 0 16px                                                                                      |
| `--deckgo-highlight-code-padding`                                  | The padding of the displayed code @default 2px 0 0                                                                                               |
| `--deckgo-highlight-code-scroll`                                   | In case you would like to change the scroll property of the shadowed code block @default auto                                                    |
| `--deckgo-highlight-code-text-align`                               | The text alignment of your code @default start                                                                                                   |
| `--deckgo-highlight-code-token-atrule`                             | Highlighted code tokens atrule, attr-value, keyword @default #ff79c6                                                                             |
| `--deckgo-highlight-code-token-comment`                            | Highlighted code tokens comment, prolog, doctype and cdata @default #6272a4                                                                      |
| `--deckgo-highlight-code-token-function`                           | Highlighted code function, class-name @default #ffb86c                                                                                           |
| `--deckgo-highlight-code-token-operator`                           | Highlighted code tokens operator, entity, url, string @default #ff79c6                                                                           |
| `--deckgo-highlight-code-token-property`                           | Highlighted code tokens property, tag, boolean, number, constant, symbol, deleted @default #bd93f9                                               |
| `--deckgo-highlight-code-token-punctuation`                        | Highlighted code token punctuation @default #6272a4                                                                                              |
| `--deckgo-highlight-code-token-regex`                              | Highlighted code tokens regex, important, variable @default #f1fa8c                                                                              |
| `--deckgo-highlight-code-token-selector`                           | Highlighted code tokens selector, attr-name, string, char, builtin, inserted @default #50fa7b                                                    |
| `--deckgo-highlight-code-ubuntu-background`                        | The background property of the host container @default #4c1e3d                                                                                   |
| `--deckgo-highlight-code-ubuntu-border`                            | The border property of the host container                                                                                                        |
| `--deckgo-highlight-code-ubuntu-border-radius`                     | The border-radius property of the host container @default 6px 6px 0 0                                                                            |
| `--deckgo-highlight-code-ubuntu-box-shadow`                        | The box-shadow property of the host container @default 2px 4px 10px rgba(0, 0, 0, 0.5)                                                           |
| `--deckgo-highlight-code-ubuntu-color`                             | The color property of the host container @default #ddd                                                                                           |
| `--deckgo-highlight-code-ubuntu-display`                           | The display property of the host container @default block                                                                                        |
| `--deckgo-highlight-code-ubuntu-header-background`                 | The background property of the card header @default linear-gradient(#504b45 0%, #3c3b37 100%)                                                    |
| `--deckgo-highlight-code-ubuntu-header-button-border-radius`       | The border-radius of a button of the card header @default 50%                                                                                    |
| `--deckgo-highlight-code-ubuntu-header-button-box-shadow`          | The box-shadow of a button of the card header @default 0px 0px 1px 0px #41403a, 0px 1px 1px 0px #474642                                          |
| `--deckgo-highlight-code-ubuntu-header-button-close-background`    | The close button background of the card header @default linear-gradient(#f37458 0%, #de4c12 100%)                                                |
| `--deckgo-highlight-code-ubuntu-header-button-close-border`        | The close button border of the card header                                                                                                       |
| `--deckgo-highlight-code-ubuntu-header-button-color`               | The color of a button of the card header @default black                                                                                          |
| `--deckgo-highlight-code-ubuntu-header-button-font-size`           | The font-size of a button of the card header @default 7px                                                                                        |
| `--deckgo-highlight-code-ubuntu-header-button-height`              | The height of a button of the card header @default 12px                                                                                          |
| `--deckgo-highlight-code-ubuntu-header-button-margin`              | The margin of a button of the card header @default 0 4px 0 0                                                                                     |
| `--deckgo-highlight-code-ubuntu-header-button-maximize-background` | The maximize button background of the card header @default linear-gradient(#7d7871 0%, #595953 100%)                                             |
| `--deckgo-highlight-code-ubuntu-header-button-maximize-border`     | The maximize button border of the card header                                                                                                    |
| `--deckgo-highlight-code-ubuntu-header-button-minimize-background` | The minimize button background of the card header @default linear-gradient(#7d7871 0%, #595953 100%)                                             |
| `--deckgo-highlight-code-ubuntu-header-button-minimize-border`     | The minimize button border of the card header                                                                                                    |
| `--deckgo-highlight-code-ubuntu-header-button-span-display`        | The Ubuntu buttons span display @default inherit                                                                                                 |
| `--deckgo-highlight-code-ubuntu-header-button-text-shadow`         | The text-shadow of a button of the card header @default 0px 1px 0px rgba(255, 255, 255, 0.2)                                                     |
| `--deckgo-highlight-code-ubuntu-header-button-width`               | The width of a button of the card header @default 12px                                                                                           |
| `--deckgo-highlight-code-ubuntu-header-font-family`                | The font-family property of the card header @default "Ubuntu"                                                                                    |
| `--deckgo-highlight-code-ubuntu-header-height`                     | The height property of the card header @default 25px                                                                                             |
| `--deckgo-highlight-code-ubuntu-header-padding`                    | The padding property of the card header. @default 0 8px                                                                                          |
| `--deckgo-highlight-code-ubuntu-header-user-color`                 | The user's color of the card header. @default #d5d0ce                                                                                            |
| `--deckgo-highlight-code-ubuntu-header-user-font-size`             | The user's font-size of the card header @default 12px                                                                                            |
| `--deckgo-highlight-code-ubuntu-header-user-line-height`           | The user's line-height of the card header @default 14px                                                                                          |
| `--deckgo-highlight-code-ubuntu-header-user-margin`                | The user's margin of the card header @default 0 0 1px 4px                                                                                        |
| `--deckgo-highlight-code-ubuntu-margin`                            | The margin property of the host container @default 16px 0                                                                                        |
| `--deckgo-highlight-code-ubuntu-overflow`                          | The overflow property of the host container @default auto                                                                                        |
| `--deckgo-highlight-code-white-space`                              | The attribute white-space of the displayed @default pre-wrap                                                                                     |
| `--deckgo-highlight-code-zoom`                                     | If you wish to manually zoom the code @default 1                                                                                                 |
| `--deckgo-lowlight-code-line-background`                           | The background of the lines you do not wish to highlight                                                                                         |
| `--deckgo-lowlight-code-line-border-bottom`                        | The border-bottom property of the lines you do not wish to highlight                                                                             |
| `--deckgo-lowlight-code-line-border-top`                           | The border-top property of the lines you do not wish to highlight                                                                                |
| `--deckgo-lowlight-code-line-color`                                | The color of the lines you do not with to highlight                                                                                              |
| `--deckgo-lowlight-code-line-font-weight`                          | The font-weight of the lines you do not wish to highlight                                                                                        |
| `--deckgo-lowlight-code-line-numbers-color`                        | The color property of the line numbers if not highlighted @default rgba(var(var(--deckgo-highlight-code-token-comment-rgb, 98, 114, 164), 0.32)) |
| `--deckgo-lowlight-code-line-opacity`                              | The opacity of the lines you do not wish to highlight @default 0.32                                                                              |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
