# DeckDeckGo - Navigation

Furthermore than the default swiping, the [DeckDeckGo] deck expose the following asynchronous methods in case you would like to add navigation features to your presentation.

## Table of contents

- [Navigation](#navigation)
  - [Go to next slide](#go-to-next-slide)
  - [Go to previous slide](#go-to-previous-slide)
  - [Go to a specific slide](#go-to-a-specific-slide)
  - [Is the deck at the begin](#is-the-deck-at-the-begin)
  - [Is the deck at the end](#is-the-deck-at-the-end)
  - [Get the index of the current slide](#get-the-index-of-the-current-slide)
  - [Get the length of the deck](#get-the-length-of-the-deck)

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

### Go to previous slide

```
await deck.slidePrev();
```

### Go to a specific slide

```
await deck.slideTo(0); // parameters: index: number, speed?: number | undefined
```

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

[DeckDeckGo]: https://deckdeckgo.com
