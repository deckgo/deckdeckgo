# Lazy loading

[DeckDeckGo] is build for performance and try to lazy load as much as possible the components of your presentation, have a look to the Lighthouse score of the [DeckDeckGo] website for reference.

In order to lazy load the images of your presentation, your could either use our dead simple component or tweak your images's attributes like following.

[DeckDeckGo] handles the lazy loading of images provided in both forms. 
 
## Lazy Image component

Here's an example of usage of our component:

```
<deckgo-lazy-img img-src="https://deckdeckgo.com/assets/img/deckdeckgo.png">
</deckgo-lazy-img>
```

## Tweak Image Tag

Instead of providing the url of your images as `src`, which would trigger an instant loading of their content, in case you would not like to use our above component, please provide their url using the attribute `data-src` instead of `src`.

```
<img data-src="https://deckdeckgo.com/assets/img/deckdeckgo.png"/>
```

[DeckDeckGo]: https://deckdeckgo.com