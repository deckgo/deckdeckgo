# Pager

[DeckDeckGo] will per default display a pager in form of a progress circle bar. It's possible to hide it or to customize the following various style options.

Also worth to notice that the pager inherits per default the document and deck direction (LTR or RTL for example).

- [Deck](#app-deck-pager-deck)
- [Customization](#app-deck-pager-customization)
- [Events](#app-deck-pager-events) 

## Show or hide

To show or hide the pager, a CSS4 variable has to be set on the `<deckgo-deck>` element. Two more options are also available when set on this element.

| Attribute                      | Default   | Description   |
| -------------------------- |-----------------|-----------------|
| --pager-display | | The display property of the pager. Set to `none` to hide it. |
| --pager-position-left | | The left attribute of the absolute positioning of the pager over the deck |
| --pager-position-right | | The right attribute of the absolute positioning of the pager over the deck |

## Customization

The following style options are available to style the pager:

| CSS4 variable                      | Default | Description |
| -------------------------- |-----------------|-----------------|
| --pager-size | 56px | |
| --pager-margin-top | 8px | |
| --pager-margin-end | 8px | |
| --pager-margin-bottom | 8px | |
| --pager-margin-start | 8px | |
| --pager-background | #eee | |
| --pager-text-color | #4c8dff | |
| --pager-text-size | 0.5em | |
| --pager-stroke-outer-width | 2.8 | |
| --pager-stroke-inner-width | 1.8 | |
| --pager-text-percentage-display | hidden | Set to `block` to display a progression with percentage (for example: 35%) |
| --pager-text-slides-display | hidden | Set to `block` to display a progression as slides' count (for example: 2/15) |

Note: of course if you would display both `--pager-text-percentage-display` and `--pager-text-slides-display` it would be a weird display, use just one at once.

# Events

In case you would like to hook on the pager click, it triggers the following event:

| Event                     | Emitted value | Description |
| -------------------------- |-----------------|-----------------|
| pagerClick | | Emitted when the user click on the pager. |

[DeckDeckGo]: https://deckdeckgo.com
