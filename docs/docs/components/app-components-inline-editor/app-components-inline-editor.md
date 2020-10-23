# WYSIWYG inline editor

The "WYSIWYG inline editor" component is an HTML floating or sticky rich text editor.

## Table of contents

- [Showcase](#app-components-inline-editor-showcase)
  - [Video](#app-components-inline-editor-video)
- [Installation](#app-components-inline-editor-installation)
  - [Using from a CDN](#app-components-inline-editor-from-a-cdn)
  - [Install from NPM](#app-components-inline-editor-from-npm)
  - [Framework integration](#app-components-inline-editor-framework-integration)
- [Usage](#app-components-inline-editor-usage)
  - [Properties](#app-components-inline-editor-properties)
  - [Custom actions slots](#app-components-inline-editor-custom-actions-slots)
  - [Styling](#app-components-inline-editor-styling)
  - [Events](#app-components-inline-editor-events)
  - [Methods](#app-components-inline-editor-methods)
  - [Examples](#app-components-inline-editor-examples)
- [Implementation](#app-components-inline-editorimplementation)

## Showcase

<div id="showcase">
  <h1 style={{color: '#3880ff'}} contenteditable slot="title">DeckDeckGo (editable title)</h1>

  <h2 style={{color: '#3880ff'}} contenteditable slot="title">The Progressive Web App alternative for simple presentations 🚀 (editable subtitle)</h2>
  
  <p style={{color: '#3880ff'}} contenteditable slot="content">Edit anywhere, display everywhere (editable paragraph)</p>
  
  <p style={{width: '200px'}} contenteditable><img style={{'max-width': '100%'}} src="https://deckdeckgo.com/assets/favicon/android-chrome-512x512.png"/></p>
 
</div>

<deckgo-inline-editor sticky-mobile="true" containers="h1,h2,h3,h4,h5,h6,p" img-editable={true}></deckgo-inline-editor>

### Video

Have a look at this video where we demonstrate how to use it!

<iframe width="560" height="315" src="https://www.youtube.com/embed/As3bXlnHHFE" frameborder="0"></iframe>

## Installation

This component could be added to your web application using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] inline editor from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/inline-editor@latest/dist/deckdeckgo-inline-editor/deckdeckgo-inline-editor.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/inline-editor
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/inline-editor';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/inline-editor/dist/loader';
deckDeckGoElement();
```

## Usage

The `<deckgo-inline-editor/>` should be added once only in your page. It will interact with all elements of types `p`, `h1`, `h2` and `h3`, or other `containers` you would define, which are set as `contenteditable`.

### Properties

The `<deckgo-inline-editor/>` expose the following properties:

| Property              | Attribute                | Description                                                                                                                                                                               | Type                  | Default                   |
| --------------------- | ------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------- | ------------------------- |
| `attachTo`            | `attach-to`              | Could be use to attach the inline editor event listeners (mousedown, touchstart and keydown) to a specific element instead of the document.                                               | `HTMLElement`         | `undefined`               |
| `containers`          | `containers`             | A comma separated list of containers where the inline editor should/could be use. Used in order to allow the component to detect some information like the current style or color.        | `string`              | `'h1,h2,h3,h4,h5,h6,div'` |
| `imgAnchor`           | `img-anchor`             | The type of element to attach the image toolbar.                                                                                                                                          | `string`              | `'img'`                   |
| `imgEditable`         | `img-editable`           | Per default, the component will not consider images as editable. Turn this option to `true` to activate the edition of images.                                                            | `boolean`             | `false`                   |
| `imgPropertyCssFloat` | `img-property-css-float` | In case you would like to use a specific property to specify the `float` on your image.                                                                                                   | `string`              | `'cssFloat'`              |
| `imgPropertyWidth`    | `img-property-width`     | In case you would like to use a specific property to specify the `width` on your image.                                                                                                   | `string`              | `'width'`                 |
| `mobile`              | `mobile`                 | The mobile mode is automatically recognize, but just it case you would like to "force" it.                                                                                                | `boolean`             | `false`                   |
| `stickyDesktop`       | `sticky-desktop`         | Use a sticky footer toolbar on desktop                                                                                                                                                    | `boolean`             | `false`                   |
| `stickyMobile`        | `sticky-mobile`          | Use a sticky footer toolbar on mobile. The sticky bar is positioned bottom except on iOS for which it will be positioned top.                                                             | `boolean`             | `false`                   |
| `customActions`       | `custom-actions`         | You might to display and add further actions to the component ? Use this property to provide a comma separated list of actions                                                            | `string`              |                           |
| `palette`             | `palette`                | In case you would like to define a custom list of colors for the palette of colors. See [@deckdeckgo/color](https://docs.deckdeckgo.com/components/color) for the default list of colors. | `DeckdeckgoPalette[]` | `DEFAULT_PALETTE`         |
| `list`                | `list`                   | Actions to manipulate the selection as list enabled?                                                                                                                                      | `boolean`             | `true`                    |
| `align`               | `align`                  | Actions to manipulate the selection alignment enabled?                                                                                                                                    | `boolean`             | `true`                    |
| `fontSize`            | `font-size`              | Actions to modify the selection font-size enabled?                                                                                                                                        | `boolean`             | `true`                    |
| `backgroundColor`     | `background-color`       | To hide the option to select a background-color                                                                                                                                           | `boolean`             | `true`                    |

### Custom actions slots

If you provide custom actions, a `slot` is going to be generated on the flight for every actions you would provide. For example:

```
<deckgo-inline-editor custom-actions="my-action">
    <span slot="my-action">My action</span>
</deckgo-inline-editor>
```

### Styling

The `<deckgo-inline-editor/>` could be styled using the following CSS4 variables which would only applies on the type `<svg/>`:

| CSS4 variable                                  | Default                        | Note                                                                                                  |
| ---------------------------------------------- | ------------------------------ | ----------------------------------------------------------------------------------------------------- |
| --deckgo-inline-editor-background-top          | white                          | The top background of the toolbar (linear gradient)                                                   |
| --deckgo-inline-editor-background-bottom       | white                          | The bottom background of the toolbar (linear gradient)                                                |
| --deckgo-inline-editor-border                  | 1px solid #3880ff              | The border of the toolbar                                                                             |
| --deckgo-inline-editor-border-radius           | 8px                            | The border radius of the toolbar                                                                      |
| --deckgo-inline-editor-padding                 | 0 8px                          | The padding of the toolbar                                                                            |
| --deckgo-inline-editor-zindex                  | 1                              | The z-Index of the toolbar                                                                            |
| --deckgo-inline-editor-transform               |                                | The transform property of the toolbar, useful for example if your viewport contains a split menu pane |
| --deckgo-inline-editor-sticky-bottom           | 0                              | The bottom attribute of the sticky toolbar                                                            |
| --deckgo-inline-editor-sticky-zindex           |                                | The z-Index of the sticky toolbar                                                                     |
| --deckgo-inline-editor-separator-background    | rgba(255, 255, 255, .2)        | The color of the separator                                                                            |
| --deckgo-inline-editor-button-color            | #3880ff                        | The buttons color                                                                                     |
| --deckgo-inline-editor-button-font-size        | 1.4rem                         | The buttons font size                                                                                 |
| --deckgo-inline-editor-button-font-family      | inherit                        | The buttons font family                                                                               |
| --deckgo-inline-editor-button-color-active     | black                          | The color of the buttons when active                                                                  |
| --deckgo-inline-editor-button-color-disabled   | #f4f5f8                        | The color of the buttons when disabled                                                                |
| --deckgo-inline-editor-button-display-disabled | none                           | Per default the disable elements on title elements are not displayed                                  |
| --deckgo-inline-editor-link-color              | #3880ff                        | The color of the input field for the url                                                              |
| --deckgo-inline-editor-link-placeholder-color  | #3880ff                        | The color of the placeholder of the input field for the url                                           |
| --deckgo-inline-editor-width                   | inherit                        | The width of the toolbar                                                                              |
| --deckgo-inline-editor-box-shadow              | 0 0 8px 4px rgba(0, 0, 0, 0.1) | A box shadow for the toolbar                                                                          |
| --deckgo-inline-editor-triangle-box-shadow     | 0 0 8px 4px rgba(0, 0, 0, 0.1) | A box shadow for the triangle above the toolbar                                                       |
| --deckgo-inline-editor-position                | absolute                       | The position of the toolbar                                                                           |

Furthermore, the following variables are also available but only have an effects on mobile devices:

| CSS4 variable                                        | Default                        | Note                         |
| ---------------------------------------------------- | ------------------------------ | ---------------------------- |
| --deckgo-inline-editor-mobile-box-shadow             | 0 0 8px 4px rgba(0, 0, 0, 0.1) | A box shadow for the toolbar |
| --deckgo-inline-editor-mobile-background-top         | #fff                           | \*\*                         |
| --deckgo-inline-editor-mobile-border                 | 0                              | \*\*                         |
| --deckgo-inline-editor-button-mobile-color           | black                          | \*\*                         |
| --deckgo-inline-editor-mobile-background-bottom      | #fff                           | \*\*                         |
| --deckgo-inline-editor-button-mobile-color-active    | #3880ff                        | \*\*                         |
| --deckgo-inline-editor-button-mobile-color-disabled  | #f4f5f8                        | \*\*                         |
| --deckgo-inline-editor-separator-mobile-background   | #f4f5f8                        | \*\*                         |
| --deckgo-inline-editor-link-mobile-color             | inherit                        | \*\*                         |
| --deckgo-inline-editor-link-mobile-placeholder-color | inherit                        | \*\*                         |

\*\* like above but for mobile

### Events

The component emits the following events:

| Event                    | Description                                                                                                                                                                              | Type                        |
| ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------- |
| `imgDidChange`           | Triggered when an image is manipulated. Note: the event won't provide directly the image but rather its container element.                                                               | `CustomEvent<HTMLElement>`  |
| `linkCreated`            | Triggered when a link is created by the user. The event detail is the container.                                                                                                         | `CustomEvent<HTMLElement>`  |
| `styleDidChange`         | Triggered when the style is modified (bold, italic, color, alignment, etc.). The event detail is the container.                                                                          | `CustomEvent<HTMLElement>`  |
| `stickyToolbarActivated` | Triggered when the sticky toolbar would be activated or not. Useful for example if you want to catch the event to hide things in your footer, as the sticky toolbar is display above it. | `CustomEvent<boolean>`      |
| `customAction`           | Triggered when a custom action is selected. Its detail provide an `action` name, the `Selection` and an `anchorLink`                                                                     | `CustomEvent<InlineAction>` |

### Methods

This component also export an async method `reset()` which will reset the inline editor (= hide it) and optionally clear its selection.

```
const element = document.querySelector('deckgo-inline-editor');
await element.reset(clearSelection: boolean, blurActiveElement?: boolean);
```

### Examples

```
<p contenteditable slot="content">This text will be editable</p>

<h1 contenteditable slot="title">This title too</h1>

<deckgo-inline-editor></deckgo-inline-editor>
```

## Implementation

Worth to notice that this Web Component does not use the obsolete [Document.execCommand()](https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand) and is therefore kind of future proof.

[deckdeckgo]: https://deckdeckgo.com
