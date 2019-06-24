# Slide: Chart

The "Chart" slide let you draw easily charts in your presentation.

## Table of contents

- [Layout](#app-slide-chart-layout)
- [Usage](#app-slide-chart-usage)
  - [Slots](#app-slide-chart-slots)
  - [Notes](#app-slide-chart-notes)
- [Chart components](#app-slide-chart-chart-components)
- [Installation](#app-slide-chart-installation)
- [Attributes](#app-slide-chart-attributes)
- [Theming](#app-slide-chart-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-chart width={200} height={100} src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100} type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
    <deckgo-slide-chart width={200} height={100}
                        type="bar" src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                        style={{'--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)', '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)'}}
                        >
      <h1 slot="title">slot="title"</h1>
    </deckgo-slide-chart>
  </deckgo-deck>
</div>

## Usage

The "Chart" slide's Web Component could be integrated using the tag `<deckgo-slide-chart/>`.

```
<deckgo-slide-chart src="./assets/csv/data-pie-chart.csv">
    <h1 slot="title">My Pie chart</h1>
</deckgo-slide-chart>
```

### Slots

The slot `title` is optional.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Chart components

The slide "Chart" relies on the charts components `<deckgo-pie-chart/>`, `<deckgo-line-chart/>` and  `<deckgo-bar-chart/>` which are described in the components [documentation](/components/charts).

## Installation

The [DeckDeckGo] charts components are provided in separate extra library. If you don't use the [DeckDeckGo] starter kit and wish to add the [DeckDeckGo] chart to your project, you will need to install and integrate it from a CDN or [npm](https://www.npmjs.com/package/@deckdeckgo/charts) as described in its [installation guide](https://docs.deckdeckgo.com/components/charts).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| type | string | pie | The type of the chart, `pie`, `line` or `bar` |
| custom-background | boolean | false | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions | boolean | false | If you would provide actions for the all deck and a specific one for this slide, set this option to `true` |

Furthermore, this slide component offers the same attributes as the [DeckDeckGo] charts Web Component, see its [documentation](/components/charts) for the details.

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |
| --slide-chart-margin-top | 0 | Margin top of the chart inside its container |
| --slide-chart-margin-end | 32px | Margin right of the chart inside its container |
| --slide-chart-margin-bottom | 64px | Margin bottom of the chart inside its container |
| --slide-chart-margin-start | 32px | Margin left of the chart inside its container |

Furthermore, this slide component offers the exact same CSS4 variables as the [DeckDeckGo] charts Web Component, see its [documentation](/components/charts) for the details.

[DeckDeckGo]: https://deckdeckgo.com