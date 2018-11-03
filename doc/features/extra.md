# DeckDeckGo - Extra Features

The [DeckDeckGo] deck expose a couple of extra features which could be added to your presentation too.

## Table of contents

- [Extra features](#extra-features)
  - [Toggle on/off the full screen mode](#toggle-onoff-the-full-screen-mode)
  - [Print the presentation](#print-the-presentation)

## Extra features

[DeckDeckGo] offers currently two extra features available on the deck element `<deckgo-deck>`.
                                                 
For example, in Vanilla Javascript, we would get a reference to the deck using the following selector:

```
const deck = document.getElementsByTagName('deckgo-deck');
```

### Toggle on/off the full screen mode

```
await deck.toggleFullScreen();
```

### Print the presentation

```
await deck.print();
```

[DeckDeckGo]: https://deckdeckgo.com
