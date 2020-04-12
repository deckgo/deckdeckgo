# Color Picker

The "Color Picker" component is a simple component to, guess what, allow your users to "pick colors" 😉

It is fully configurable in terms of colors, you could define the set of colors you rather like to offer and it also implements a "more" action which, if clicked, will open the platform standard color picker.

## Table of contents

- [Showcase](#app-components-color-showcase)
- [Installation](#app-components-color-installation) - [Using from a CDN](#app-components-color-from-a-cdn) - [Install from NPM](#app-components-color-from-npm) - [Framework integration](#app-components-color-framework-integration)
- [Usage](#app-components-color-usage)
  - [Slots](#app-components-color-slots)
  - [Attributes](#app-components-color-attributes)
  - [Theming](#app-components-color-theming)
  - [Methods](#app-components-color-methods)
- [Trying it out](#app-components-color-trying-it-out)

## Showcase

<div>
    <deckgo-color>
      <span slot="more">...</span>
    </deckgo-color>
</div>

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/color@latest/dist/deckdeckgo-color/deckdeckgo-color.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/color@latest/dist/deckdeckgo-color/deckdeckgo-color.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/color
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/color';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/color/dist/loader';
deckDeckGoElement();
```

## Usage

The "Color Picker" Web Component could be integrated using the tag `<deckgo-color/>`.

```
<deckgo-color>
  <span slot="more">...</span>
</deckgo-color>
```

### Slots

The slot `more` is optional, moreover, the "more" action itself could be turned off.

### Attributes

This component offers the following options which could be set using attributes:

| Property   | Attribute   | Description                                               | Type                  | Default           |
| ---------- | ----------- | --------------------------------------------------------- | --------------------- | ----------------- |
| `colorHex` | `color-hex` | The current selected color provided as hexadecimal value. | `string`              | `undefined`       |
| `colorRgb` | `color-rgb` | The current selected color provided as a rgb value.       | `string`              | `undefined`       |
| `more`     | `more`      | In case you would not like to offer the "more" options.   | `boolean`             | `true`            |
| `moreAlt`  | `more-alt`  | An accessibility label for the "more action.              | `string`              | `'More'`          |
| `palette`  |             | The palette of color (see here under).                    | `DeckdeckgoPalette[]` | `DEFAULT_PALETTE` |

#### Palette

The `palette` attribute is a complex object and therefore could only be set using Javascript.

It is defined as the following:

```
export interface DeckdeckgoPaletteColor {
    hex: string;
    rgb?: string;
}

export interface DeckdeckgoPalette {
  color: DeckdeckgoPaletteColor;
  alt?: string;
}
```

The key value is the color provided as `hex` value. The `rgb` value is use for presentation purpose, for the hover action and the highlight of the selected color. If you wish to highlight a selected color, you could either provide `color-hex` or `color-rgb`.

The default palette is the following:

```
export const DEFAULT_PALETTE: DeckdeckgoPalette[] = [
  {
    color: {
      hex: '#FF6900',
      rgb: '255,105,0'
    },
    alt: 'Orange'
  },
  {
    color: {
      hex: '#FCB900',
      rgb: '252,185,0'
    },
    alt: 'Yellow'
  },
  {
    color: {
      hex: '#7BDCB5',
      rgb: '123,220,181'
    },
    alt: 'Light green'
  },
  {
    color: {
      hex: '#00D084',
      rgb: '0,208,132'
    },
    alt: 'Green'
  },
  {
    color: {
      hex: '#8ED1FC',
      rgb: '142,209,252'
    },
    alt: 'Light blue'
  },
  {
    color: {
      hex: '#0693E3',
      rgb: '6,147,227'
    },
    alt: 'Blue'
  },
  {
    color: {
      hex: '#ABB8C3',
      rgb: '171,184,195'
    },
    alt: 'Grey'
  },
  {
    color: {
      hex: '#EB144C',
      rgb: '235,20,76'
    },
    alt: 'Red'
  },
  {
    color: {
      hex: '#F78DA7',
      rgb: '247,141,167'
    },
    alt: 'Pink'
  },
  {
    color: {
      hex: '#9900EF',
      rgb: '153,0,239'
    },
    alt: 'Violet'
  },
  {
    color: {
      hex: '#000000',
      rgb: '0,0,0'
    },
    alt: 'Black'
  }
];
```

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default     | Note                                                          |
| ---------------------------------- | ----------- | ------------------------------------------------------------- |
| --deckgo-button-width              | 28px        | The width of a button to select a color and the more button   |
| --deckgo-button-height             | 28px        | The height of a button to select a color and the more button  |
| --deckgo-button-margin             | 4px         | The margin of a button to select a color and the more button  |
| --deckgo-button-outline            | none        | The outline of a button to select a color and the more button |
| --deckgo-button-border             | none        | The border of a button to select a color and the more button  |
| --deckgo-button-border-radius      | 50%         | The border radius of a button to select a color               |
| --deckgo-button-more-border-radius | 50%         | The border radius of the more button                          |
| --deckgo-button-more-border        | none        | The border of the more button                                 |
| --deckgo-button-more-outline       | none        | The outline of the more button                                |
| --deckgo-flex-wrap                 | wrap        | Wrap properties of the buttons' container                     |
| --deckgo-overflow                  | visible     | Overflow property of the buttons's container                  |
| --deckgo-button-more-background    | transparent | The background of the more button                             |

### Events

To listen to the selected color you have to subscribe to the following event:

| Event         | Description              | Type                                  |
| ------------- | ------------------------ | ------------------------------------- |
| `colorChange` | Emit the selected color. | `CustomEvent<DeckdeckgoPaletteColor>` |

In case the platform color picker would be use by the user, the change will be triggered multiple times, as long as the user change its value in the platform picker.

For the definition of the type of the event, see above description of `DeckdeckgoPaletteColor`.

[deckdeckgo]: https://deckdeckgo.com
