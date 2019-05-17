# Extra Features

The [DeckDeckGo] deck exposes a couple of extra features which could be added to your presentation too if you don't already use the starter kit.

## Table of contents

- [Toggle on/off the full screen mode](#app-deck-extra-features-toggle-onoff-the-full-screen-mode)
- [Print the presentation](#app-deck-extra-features-print-the-presentation)
- [Mobile](#app-deck-extra-features-mobile)
- [initSlideSize](#app-deck-extra-features-init-slide-size)
- [loadBackground](#app-deck-extra-features-load-background)

## Toggle on/off the full screen mode

```
await deck.toggleFullScreen();
```

## Print the presentation

```
await deck.print();
```

## Mobile

A util method to know if the current presentation is browsed on a mobile device or not.

```
await deck.isMobile(); // resolve a boolean
```

## Init slide size

In case you would like to recalculate the slides' size (width and height)

```
await deck.initSlideSize();
```

## Load background

If you would dynamically change the deck background element you could forward that changes to also slides using the following method:  

```
await deck.loadBackground();
```

[DeckDeckGo]: https://deckdeckgo.com
