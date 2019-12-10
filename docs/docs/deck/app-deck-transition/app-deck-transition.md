# Transition

The transition between the [DeckDeckGo] slides could be animated in different ways.

Per default, the animation is the `slide` effect.  

## Properties

A specific effect could be set using the following properties of the root element `<deckgo-deck/>`:

| Property              | Attribute                | Description | Type          | Default                   |
| --------------------- | ------------------------ | ----------- | ------------- | ------------------------- |
| `transition`            | `transition`              | The animation effect between slides. | `slide`, `fade` or `none` | `slide` |               |

#### Styling

It is also possible to style the `fade` transition using the following CSS4 variables:

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --transition-fade-duration | 500ms | The duration of the animation. |
| --transition-fade-hidden-opacity | 0.4 | The base opacity when the slide is not displayed. |
| --slide-transition | | An optional transition effect for each slide container. |

[DeckDeckGo]: https://deckdeckgo.com
