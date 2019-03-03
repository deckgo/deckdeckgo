# Navigation

If you use the [DeckDeckGo] starter kit, the navigation methods are already bind out of the box.

However, if you wish to know more about the different options or are using the core of [DeckDeckGo], the [DeckDeckGo] deck exposes the following asynchronous methods in case you would like to add navigation features to your project.

- [Introduction](#app-deck-navigation-introduction)
- [Go to next slide](#app-deck-navigation-go-to-next-slide)
  - [Optional parameters](#app-deck-navigation-optional-parameters)
- [Go to previous slide](#app-deck-navigation-go-to-previous-slide)
  - [Optional parameters](#app-deck-navigation-optional-parameters-1)
- [Go to a specific slide](#app-deck-navigation-go-to-a-specific-slide)
  - [Parameters](#app-deck-navigation-parameters)
- [Is the deck at the begin](#app-deck-navigation-is-the-deck-at-the-begin)
- [Is the deck at the end](#app-deck-navigation-is-the-deck-at-the-end)
- [Get the index of the current slide](#app-deck-navigation-get-the-index-of-the-current-slide)
- [Get the length of the deck](#app-deck-navigation-get-the-length-of-the-deck)

## Introduction

In the following examples we are accessing the features available on the deck element `<deckgo-deck>`.
 
For example, in Vanilla Javascript, we would get a reference to the deck using the following selector:

```
const deck = document.getElementsByTagName('deckgo-deck');
```

## Go to next slide

```
await deck.slideNext();
```

### Optional parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |-----------------|-----------------|-----------------|
| slideAnimation | boolean | true | Set to `false` in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed. |
| emitEvent | boolean | true | Set to `false` in case you would not like the events `slideNextDidChange` and `slidePrevDidChange` to be fired. Note that to use this parameter, the previous should be set too. |

For example:

```
await deck.slideNext(false, false);
```

## Go to previous slide

```
await deck.slidePrev();
```

### Optional parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |-----------------|-----------------|-----------------|
| slideAnimation | boolean | true | Set to `false` in case you would not like the inner animation of a slide, like the reveal or code animation for example, to be performed. |
| emitEvent | boolean | true | Set to `false` in case you would not like the events `slideNextDidChange` and `slidePrevDidChange` to be fired. Note that to use this parameter, the previous should be set too. |

For example:

```
await deck.slidePrev(false, false);
```

## Go to a specific slide

```
await deck.slideTo(0); // parameters: index: number, speed?: number | undefined
```

### Parameters

| Parameter                      | Type | Default | Description |
| -------------------------- |-----------------|-----------------|-----------------|
| index | number | | Slide index of the specific slide. |
| speed | number | 300 | The slide transition speed. Default 300ms. |
| emitEvent | boolean | true | In case you would not like to emit the event `slideToChange`. Note that if you would use this parameter, the above `speed` parameter must be provided too. |

## Is the deck at the begin

```
await deck.isBeginning(); // resolve a boolean
```

## Is the deck at the end

```
await deck.isEnd(); // resolve a boolean
```

## Get the index of the current slide 

```
await deck.getActiveIndex(); // resolve a number
```

## Get the length of the deck

```
await deck.getLength(); // resolve a number
```

[DeckDeckGo]: https://deckdeckgo.com
