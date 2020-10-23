# Slide: Countdown

The "Countdown" slide displays a countdown until your presentation starts.

It could be handy, for example when you organize a meetup, to display a countdown until the event start.

## Table of contents

- [Layout](#app-slide-countdown-layout)
- [Installation](#app-slide-countdown-installation)
  - [From a CDN](#app-slide-countdown-from-a-cdn)
  - [From NPM](#app-slide-countdown-from-npm)
  - [Framework integration](#app-slide-countdown-framework-integration)
- [Usage](#app-slide-countdown-usage)
  - [Usage](#app-slide-countdown-usage-1)
  - [Slots](#app-slide-countdown-slots)
- [Attributes](#app-slide-countdown-attributes)
  - [Example without any slots](#app-slide-countdown-example-without-any-slots)
- [Theming](#app-slide-countdown-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-countdown hours={1} minutes={0} seconds={5}>
        <h1 slot="title">slot="title"</h1>
        <p slot="hours">slot="hours"</p>
        <p slot="minutes">slot="minutes"</p>
        <p slot="seconds">slot="seconds"</p>
    </deckgo-slide-countdown>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-countdown@latest/dist/deckdeckgo-slide-countdown/deckdeckgo-slide-countdown.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-countdown) run the following command:

```bash
npm install @deckdeckgo/slide-countdown
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-countdown';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-countdown/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Countdown" slide's Web Component could be integrated using the tag `<deckgo-slide-countdown/>`.

### Usage

```
<deckgo-deck>
    <deckgo-slide-countdown until="2019-08-05T23:25:59.000+02:00">
        <h1 slot="title">My presentation starts at</h1>
        <p slot="days">Days</p>
        <p slot="hours">Hours</p>
        <p slot="minutes">Minutes</p>
        <p slot="seconds">Seconds</p>
    </deckgo-slide-countdown>
</deckgo-deck>
```

### Slots

The slots `title` as well as `days`, `hours`, `minutes` and `seconds` are optional.

## Attributes

The time until your presentation should be provided to render the countdown. This value could be either passed through attributes `days`, `hours`, `minutes` and `seconds` or as a particular date using `until`.

This components exposes the following attributes:

| Attribute         | Type    | Default | Description                                                                                                     |
| ----------------- | ------- | ------- | --------------------------------------------------------------------------------------------------------------- |
| days              | number  | 0       | The amount of days before your presentations (max. 99 will be displayed)                                        |
| hours             | number  | 0       | The amount of hours before your presentations (max. 23)                                                         |
| minutes           | number  | 0       | The amount of minutes before your presentations (max. 59)                                                       |
| seconds           | number  | 0       | The amount of seconds before your presentations (max. 59)                                                       |
| until             | string  |         | A specific date and time until when your presentation will start                                                |
| custom-background | boolean | false   | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false   | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

If you would provide a date using `until`, note that the format should be provided as a valid and parsable date. See [Date.parse()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/parse) for more information about the format.

### Example without any slots

```
<deckgo-deck>
  <deckgo-slide-countdown hours="0" minutes="10" seconds="45">
  </deckgo-slide-countdown>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                              | Default                                                                   | Note                                                        |
| ------------------------------------------ | ------------------------------------------------------------------------- | ----------------------------------------------------------- |
| --background                               |                                                                           |                                                             |
| --color                                    |                                                                           |                                                             |
| --slide-padding-top                        | 16px                                                                      | Padding top of the all slide                                |
| --slide-padding-end                        | 32px                                                                      | Padding right of the all slide                              |
| --slide-padding-bottom                     | 16px                                                                      | Padding bottom of the all slide                             |
| --slide-padding-start                      | 32px                                                                      | Padding left of the all slide                               |
| --slide-countdown-container-padding-bottom | 64px                                                                      | The bottom padding of the displayed time container          |
| --slide-countdown-digits-max-width         | 36em                                                                      | The max width of the container containing all digits        |
| --slide-countdown-digits-minmax-width      | 12em                                                                      | The grid minmax value of one of the three columns of digits |
| --slide-countdown-digits-width             | 4em                                                                       | The width of one of the (two) digits                        |
| --slide-countdown-digits-height            | 4m                                                                        | The height of the (two) digits                              |
| --slide-countdown-digits-background        |                                                                           | The background color of the (two) digits                    |
| --slide-countdown-digits-border-radius     |                                                                           | The border-radius of the (two) digits                       |
| --slide-countdown-digits-box-shadow        | 0 3px 4px 0 rgba(0, 0, 0, .2), inset 2px 4px 0 0 rgba(255, 255, 255, .08) | The box-shadow of the (two) digits                          |
| --slide-countdown-digit-margin-right       | 0.625em                                                                   | The space between two digits                                |
| --slide-countdown-digits-font-size         | 3em                                                                       | The border-radius of the (two) digits                       |
| --slide-countdown-digits-font-weight       |                                                                           | The font-weight of the (two) digits                         |
| --slide-countdown-digits-color             |                                                                           | The color of the (two) digits                               |

[deckdeckgo]: https://deckdeckgo.com
