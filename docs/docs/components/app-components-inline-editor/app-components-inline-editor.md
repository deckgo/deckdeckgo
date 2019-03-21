# WYSIWYG inline editor

The "WYSIWYG inline editor" component is an extra component which will be use in the upcoming [DeckDeckGo] Studio.

## Table of contents

- [Showcase](#app-components-qrcode-showcase)
- [Getting started](#app-components-qrcode-getting-started)
	- [Using from a CDN](#app-components-from-a-cdn)
	- [Install from NPM](#app-components-from-npm)
	- [Framework integration](#app-components-qrcode-framework-integration)
- [Usage](#app-components-qrcode-usage)
	- [Properties](#app-components-qrcode-properties)
	- [Styling](#app-components-qrcode-styling)
	- [Examples](#app-components-qrcode-examples)

## Showcase

<div>
  <h1 style={{color: '#3880ff'}} contenteditable>DeckDeckGo (editable title)</h1>

  <h2 style={{color: '#3880ff'}} contenteditable>The Progressive Web App alternative for simple presentations 🚀 (editable subtitle)</h2>
  
  <p style={{color: '#3880ff'}} contenteditable>Edit anywhere, display everywhere (editable paragraph)</p>
</div>

<deckgo-inline-editor sticky-desktop="true" sticky-mobile="true"></deckgo-inline-editor>

## Getting started

This Web Component is an inline WYSIWYG HTML Editor, It creates a floating editor bar or a sticky footer bar that shows up when you select a piece of text of your page.

To add the component to your web applications, it could be use directly in your project from a CDN, using a simple script include, or could be installed from [npm](https://www.npmjs.com/package/deckdeckgo-qrcode).

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] inline editor from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script src="https://unpkg.com/deckdeckgo-inline-editor@latest/dist/deckdeckgo-inline-editor.js"></script>
```
### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/deckdeckgo-qrcode) using the following command:

```bash
npm install deckdeckgo-inline-editor
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

## Usage

The `<deckgo-inline-editor/>` should be added once only in your page. It will interact with all elements of types `p`, `h1`, `h2`  and `h3` you would have set as `contenteditable`.

### Properties

The `<deckgo-inline-editor/>` expose the following properties:

| Property              | Attribute               | Description | Type      | Default     |
| --------------------- | ----------------------- | ----------- | --------- | ----------- |
| `mobile`              | `mobile`                | The mobile mode is automatically recognize, but just it case you would like to "force" it            | `boolean` | `false` or `true` according the device    |
| `stickyDesktop`       | `sticky-desktop`        | Use a sticky footer toolbar on desktop            | `boolean` | `false`     |
| `stickyMobile`        | `sticky-mobile`         | Use a sticky footer toolbar on mobile. Note: except iOS, feel free to send a PR if you know how to handle this with the software keyboard            | `boolean` | `false`     |
| `attachTo`            | `attach-to`             | Could be use to attach the inline editor event listeners (mousedown, touchstart and keydown) to a specific element instead of the document | `HTMLElement` |     |

### Styling

The `<deckgo-inline-editor/>` could be styled using the following CSS4 variables which would only applies on the type `<svg/>`:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-inline-editor-background-top | white | The top background of the toolbar (linear gradient) |
| --deckgo-inline-editor-background-bottom | white | The bottom background of the toolbar (linear gradient) |
| --deckgo-inline-editor-border | 1px solid #3880ff | The border of the toolbar | 
| --deckgo-inline-editor-border-radius | 8px | The border radius of the toolbar |
| --deckgo-inline-editor-padding | 0 8px | The padding of the toolbar |
| --deckgo-inline-editor-zindex | 1 | The z-Index of the toolbar |
| --deckgo-inline-editor-transform | | The transform property of the toolbar, useful for example if your viewport contains a split menu pane |
| --deckgo-inline-editor-sticky-bottom | 0 | The bottom attribute of the sticky toolbar |
| --deckgo-inline-editor-separator-background | rgba(255, 255, 255, .2) | The color of the separator |
| --deckgo-inline-editor-button-color | #3880ff | The buttons color |
| --deckgo-inline-editor-button-font-size | 1.4rem | The buttons font size |
| --deckgo-inline-editor-button-font-family | inherit | The buttons font family |
| --deckgo-inline-editor-button-color-active | black | The color of the buttons when active |
| --deckgo-inline-editor-button-color-disabled | #f4f5f8 | The color of the buttons when disabled  |
| --deckgo-inline-editor-link-color | #3880ff | The color of the input field for the url |
| --deckgo-inline-editor-link-placeholder-color | #3880ff | Ther color of the placeholder of the input field for the url |

Furthermore, the following variables are also available but only have an effects on mobile devices:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-inline-editor-mobile-box-shadow | 0 0px 1px rgba(0, 0, 0, 0.16), 0 1px 3px rgba(0, 0, 0, 0.15) | A box shadow for the toolbar |
| --deckgo-inline-editor-mobile-background-top | #fff | ** |
| --deckgo-inline-editor-mobile-border | 0 | ** |
| --deckgo-inline-editor-button-mobile-color | black | ** |
| --deckgo-inline-editor-mobile-background-bottom | #fff | ** |
| --deckgo-inline-editor-button-mobile-color-active | #3880ff | ** |
| --deckgo-inline-editor-button-mobile-color-disabled | #f4f5f8 | ** |
| --deckgo-inline-editor-separator-mobile-background | #f4f5f8 | ** |
| --deckgo-inline-editor-link-mobile-color | inherit | ** |
| --deckgo-inline-editor-link-mobile-placeholder-color | inherit | ** |

** like above but for mobile

### Slot

The icon for the link action should be provided using a dedicated slot name `link`

```
<deckgo-inline-editor>
  <span slot="link">L</span>
</deckgo-inline-editor>
```

### Examples

```
<p contenteditable>This text will be editable</p>

<h1 contenteditable>This title too</h1>

<deckgo-inline-editor></deckgo-inline-editor>
```

[DeckDeckGo]: https://deckdeckgo.com 