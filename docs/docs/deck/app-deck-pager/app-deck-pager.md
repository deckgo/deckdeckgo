# Pager

[DeckDeckGo] will per default display a pager in form of a progress circle bar. It's possible to hide it or to customize the following various style options.

Also worth to notice that the pager inherits per default the document and deck direction (LTR or RTL for example).

- [Show or hide](#app-deck-pager-show-or-hide)
- [Customization](#app-deck-pager-customization)

## Show or hide

The show or hide options of the pager are available on the `<deckgo-deck>` element.

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| pager | boolean | true | Show or hide the pager |
| pagerPercentage | boolean | true | Show or hide the progression in percentage inside the pager |

## Customization

The following style options are available to style the pager:

| CSS4 variable                      | Default |
| -------------------------- |-----------------|
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
| --pager-position-left | |
| --pager-position-right | |

[DeckDeckGo]: https://deckdeckgo.com
