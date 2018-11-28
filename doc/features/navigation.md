# DeckDeckGo - Navigation

Furthermore than the default swiping, the [DeckDeckGo] deck expose the following asynchronous methods in case you would like to add navigation features to your presentation.

## Table of contents

- [Navigation](#navigation)
  - [Go to next slide](#go-to-next-slide)
    - [Optional parameters](#pptional-parameters)
  - [Go to previous slide](#go-to-previous-slide)
    - [Optional parameters](#optional-parameters)
  - [Go to a specific slide](#go-to-a-specific-slide)
      - [Parameters](#parameters)
  - [Is the deck at the begin](#is-the-deck-at-the-begin)
  - [Is the deck at the end](#is-the-deck-at-the-end)
  - [Get the index of the current slide](#get-the-index-of-the-current-slide)
  - [Get the length of the deck](#get-the-length-of-the-deck)
- [Pager](#pager)
  - [Show or hide](#show-or-hide)
  - [Customization](#customization)

## Navigation

In the following examples we are accessing the features available on the deck element `<deckgo-deck>`.
 
For example, in Vanilla Javascript, we would get a reference to the deck using the following selector:

```
const deck = document.getElementsByTagName('deckgo-deck');
```

### Go to next slide

```
await deck.slideNext();
```

For example:

#### Optional parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| slideAnimation | boolean | true | Set to `false` in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed. |
| emitEvent | boolean | true | Set to `false` in case you would not like the events `slideNextDidChange` and `slidePrevDidChange` to be fired. Note that to use this parameter, the previous should be set too. |

```
await deck.slideNext(false, false);
```

### Go to previous slide

```
await deck.slidePrev();
```

#### Optional parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| slideAnimation | boolean | true | Set to `false` in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed. |
| emitEvent | boolean | true | Set to `false` in case you would not like the events `slideNextDidChange` and `slidePrevDidChange` to be fired. Note that to use this parameter, the previous should be set too. |

For example:

```
await deck.slidePrev(false, false);
```

### Go to a specific slide

```
await deck.slideTo(0); // parameters: index: number, speed?: number | undefined
```

#### Parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| index | number | | Slide index of the specific slide. |
| speed | number | 300 | The slide transition speed. Default 300ms. |
| emitEvent | boolean | true | In case you would not like to emit the event `slideToChange`. Note that if you would use this parameter, the above `speed` parameter must be provided too. |

### Is the deck at the begin

```
await deck.isBeginning(); // resolve a boolean
```

### Is the deck at the end

```
await deck.isEnd(); // resolve a boolean
```

### Get the index of the current slide 

```
await deck.getActiveIndex(); // resolve a number
```

### Get the length of the deck

```
await deck.getLength(); // resolve a number
```

## Pager

[DeckDeckGo] will per default display a pager in form of a progress circle bar. It's possible to hide it or to customize the following various style options.

### Show or hide

The show or hide options of the pager are available on the `<deckgo-deck>` element.

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| pager | boolean | true | Show or hide the pager |
| pagerPercentage | boolean | true | Show or hide the progression in percentage inside the pager |

### Customization

The following style options are available to style the pager:

| CSS4 variable                      | Default |
| -------------------------- |:-----------------:|
| --pager-size | 56px |
| --pager-margin-top | 8px |
| --pager-margin-end | 8px |
| --pager-margin-bottom | 8px |
| --pager-margin-start | 8px |
| --pager-background | #eee |
| --pager-text-color | #4c8dff |
| --pager-text-size | 0.5em |
| --pager-stroke-outer-width | 2.8 |
| --pager-stroke-inner-width | 1.8 |

[DeckDeckGo]: https://deckdeckgo.com
