# Extra Features

The [DeckDeckGo] deck exposes a couple of extra features which could be added to your presentation too if you don't already use the starter kit.

## Table of contents

- [Toggle on/off the full screen mode](#app-deck-extra-features-toggle-onoff-the-full-screen-mode)
- [Print the presentation](#app-deck-extra-features-print-the-presentation)
- [Mobile](#app-deck-extra-features-mobile)
- [initSlideSize](#app-deck-extra-features-init-slide-size)
- [loadBackground](#app-deck-extra-features-load-background)
- [lazyLoadAllContent](#app-deck-extra-features-lazy-load-all-content)

## Toggle on/off the full screen mode

```
await deck.toggleFullScreen();
```

## Print the presentation

```
await deck.print();
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

## Lazy load all content

To load the content of each and every slides of the deck which would normally be lazy loaded when you would swipe through your slides.

```
await deck.lazyLoadAllContent();
```

[deckdeckgo]: https://deckdeckgo.com
