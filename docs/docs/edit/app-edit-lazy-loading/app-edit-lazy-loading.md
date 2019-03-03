# Lazy loading

[DeckDeckGo] is build for performance and try to lazy load as much as possible the components of your presentation, have a look to the Lighthouse score of the [DeckDeckGo] website for reference.

Therefore, in order to lazy load the images of your presentation, please provide their url using the attribute `data-src` instead of `src`. [DeckDeckGo] will then take care of loading them only when needed.

```
<img data-src="https://deckdeckgo.com/assets/img/deckdeckgo.png"/>
```

[DeckDeckGo]: https://deckdeckgo.com