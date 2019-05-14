# Lazy Image

The "Lazy Image" component is a dead simple component to lazy load images.

It leverages the IntersectionObserver API to lazy load images and also allows you to trigger "manually" their loading.

## Table of contents

- [Usage](#app-components-lazy-img-usage)
  - [Slots](#app-components-lazy-img-slots)
  - [Attributes](#app-components-lazy-img-attributes)
  - [Theming](#app-components-lazy-img-theming)
  - [Methods](#app-components-lazy-img-methods)

## Usage

The "Lazy Image" Web Component could be integrated using the tag `<deckgo-lazy-img/>`.

```
<deckgo-lazy-img img-src="/assets/twitter.svg">
</deckgo-lazy-img>
```

### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| img-src | string |  | The image source (= URI) to lazy load |
| img-alt | string |  | The image alternate text |
| observer-root-margin | string | 100px 0px | A string which specifies a set of offsets to add to the root's bounding_box when calculating intersections, effectively shrinking or growing the root for calculation purposes. [More info.](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver) |
| observer-threshold | number or number[] |  | Either a single number or an array of numbers between 0.0 and 1.0, specifying a ratio of intersection area to total bounding box area for the observed target. [More info.](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver/IntersectionObserver) |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --deckgo-lazy-img-max-width | 100% | Image max width |
| --deckgo-lazy-img-min-width |  | Image min width |
| --deckgo-lazy-img-pointer-events | none | Image pointer events |
| --deckgo-lazy-img-height | | Image height |
| --deckgo-lazy-img-width | | Image width |
| --deckgo-lazy-img-float | | Image float |
| --deckgo-lazy-img-padding | | Image padding |

### Methods

This component also expost an async method `lazyLoad()` in case you would like to trigger "manually" the loading of the image.

```
const element = document.querySelector('deckgo-lazy-img');
await element.lazyLoad();
```

[DeckDeckGo]: https://deckdeckgo.com 