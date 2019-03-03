# Events

The [DeckDeckGo] deck triggers the following events:

| Event                     | Emitted value | Description |
| -------------------------- |-----------------|-----------------|
| slidesDidLoad | string[] | Emitted when the deck and all slides have loaded. Emit the an ordered list of all the tag names of the slides. |
| slideNextDidChange | number | Emitted when the next slide has started. Emit the index of the new active slide. |
| slidePrevDidChange | number | Emitted when the previous slide has ended. Emit the index of the new active slide. |
| slideToChange | number | Emitted when a specific slide has been selected. Emit the index of the new selected slide. |
| slideDrag | number | Emitted when the slider is actively being moved. Emit the transformX value of the deck. |
| slideWillChange | number | Emitted before the active slide has changed. Emit the transformX value of the deck. |

[DeckDeckGo]: https://deckdeckgo.com
