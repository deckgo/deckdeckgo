# deckgo-color

The "Color Picker" component is a simple component to, guess what, pick colors 😉.

## Installation

This component can be added to your web application with following methods.

> If you are using our developer kit to create a presention, this component is already included

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/color@latest/dist/deckdeckgo-color/deckdeckgo-color.esm.js"></script>
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
        hex: '#8ED1FC',
        rgb: '142,209,252',
      },
      alt: 'Light blue',
    },
    {
      color: {
        hex: '#0693E3',
        rgb: '6,147,227',
      },
      alt: 'Blue',
    },
    {
      color: {
        hex: '#7BDCB5',
        rgb: '123,220,181',
      },
      alt: 'Light green',
    },
    {
      color: {
        hex: '#00D084',
        rgb: '0,208,132',
      },
      alt: 'Green',
    },
    {
      color: {
        hex: '#FCB900',
        rgb: '252,185,0',
      },
      alt: 'Yellow',
    },
    {
      color: {
        hex: '#FF6900',
        rgb: '255,105,0',
      },
      alt: 'Orange',
    },
    {
      color: {
        hex: '#F78DA7',
        rgb: '247,141,167',
      },
      alt: 'Pink',
    },
    {
      color: {
        hex: '#EB144C',
        rgb: '235,20,76',
      },
      alt: 'Red',
    },
    {
      color: {
        hex: '#ffffff',
        rgb: '255,255,255',
      },
      alt: 'White',
      display: {
        borderColor: '#ddd',
        boxShadowColor: '221,221,221',
      },
    },
    {
      color: {
        hex: '#ABB8C3',
        rgb: '171,184,195',
      },
      alt: 'Grey',
    },
    {
      color: {
        hex: '#000000',
        rgb: '0,0,0',
      },
      alt: 'Black',
    },
];
```

<!-- Auto Generated Below -->


## Properties

| Property   | Attribute   | Description                                                                                                      | Type                  | Default                 |
| ---------- | ----------- | ---------------------------------------------------------------------------------------------------------------- | --------------------- | ----------------------- |
| `colorHex` | `color-hex` | The current selected color provided as hexadecimal value                                                         | `string`              | `undefined`             |
| `colorRgb` | `color-rgb` | The current selected color provided as a rgb value (without "rgb()", only value such as for example 255, 67, 54) | `string`              | `undefined`             |
| `inputAlt` | `input-alt` | An accessibility label for the color input field                                                                 | `string`              | `'Input a color (hex)'` |
| `palette`  | --          | The palette of color.                                                                                            | `DeckdeckgoPalette[]` | `DEFAULT_PALETTE`       |


## Events

| Event         | Description             | Type                                  |
| ------------- | ----------------------- | ------------------------------------- |
| `colorChange` | Emit the selected color | `CustomEvent<DeckdeckgoPaletteColor>` |


## CSS Custom Properties

| Name                                  | Description                                                            |
| ------------------------------------- | ---------------------------------------------------------------------- |
| `--deckgo-color-button-border-radius` | Button border-radius @default 2px                                      |
| `--deckgo-color-button-height`        | Button height @default 28px                                            |
| `--deckgo-color-button-margin`        | Button margin @default 4px                                             |
| `--deckgo-color-button-outline`       | Button outline @default none                                           |
| `--deckgo-color-button-width`         | Button width @default 28px                                             |
| `--deckgo-color-flex-wrap`            | Component flex-wrap @default wrap                                      |
| `--deckgo-color-hash-background`      | Hash (#) background @default rgb(240, 240, 240) none repeat scroll 0 0 |
| `--deckgo-color-hash-border-radius`   | Hash (#) border-radius @default 4px 0 0 4px                            |
| `--deckgo-color-hash-color`           | Hash (#) color @default rgb(152, 161, 164)                             |
| `--deckgo-color-hash-height`          | Hash (#) height @default 28px                                          |
| `--deckgo-color-hash-width`           | Hash (#) width @default 28px                                           |
| `--deckgo-color-input-background`     | Input background @default inherit                                      |
| `--deckgo-color-input-border-radius`  | Input border-radius @default 0 4px 4px 0                               |
| `--deckgo-color-input-box-shadow`     | Input box-shadow @default rgb(240, 240, 240) 0 0 0 1px inset           |
| `--deckgo-color-input-color`          | Input color @default rgb(102, 102, 102)                                |
| `--deckgo-color-input-font-family`    | Input font-family @default inherit                                     |
| `--deckgo-color-input-height`         | Input height @default 28px                                             |
| `--deckgo-color-input-margin`         | Input field margin @default 4px                                        |
| `--deckgo-color-input-max-width`      | Input max-width @default 136px                                         |
| `--deckgo-color-input-padding`        | Input padding @default 0 4px                                           |
| `--deckgo-color-overflow`             | Component overflow @default visible                                    |
| `--deckgo-color-padding`              | Component padding @default 8px                                         |


## Dependencies

### Depends on

- [deckgo-color-input](../input)

### Graph
```mermaid
graph TD;
  deckgo-color --> deckgo-color-input
  style deckgo-color fill:#f9f,stroke:#333,stroke-width:4px
```

----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
