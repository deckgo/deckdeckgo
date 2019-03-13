# Slide: Split

The "Split" slide is a simple slide which display two panes on the page.

## Table of contents

- [Layout](#app-slide-split-layout)
- [Usage](#app-slide-split-usage)
  - [Slots](#app-slide-split-slots)
  - [Notes](#app-slide-split-notes)
- [Attributes](#app-slide-split-attributes)
- [Theming](#app-slide-split-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-split>
        <h1 slot="title">Two columns subject</h1>
        <p slot="start">
          The content you want to display on the left side of the page
        </p>
        <p slot="end">
          The content you want to display on the right side of the page
        </p>
      </deckgo-slide-split>
  </deckgo-deck>
</div>

## Usage

The "Split" slide's Web Component could be integrated using the tag `<deckgo-slide-split/>`.

```
<deckgo-deck>
  <deckgo-slide-split>
    <h1 slot="title">Two columns subject</h1>
    <p slot="start">
      The content you want to display on the left side of the page
    </p>
    <p slot="end">
      The content you want to display on the right side of the page
    </p>
  </deckgo-slide-split>
</deckgo-deck>
```

### Slots

Both slots `title`, `start` and `end` are optional. Without providing one of them, the page will remain empty.

The `start` slot is the content of the left pane respectively the slot `end` is the content of the right pane.

Note: The slot `title` is per default hidden even if you provide it. See attributes below if you wish to display it. 

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-split-background-start | | Left split pane background |
| --slide-split-color-start | | Left split pane color |
| --slide-split-background-end | | Right split pane background |
| --slide-split-color-end | | Right split pane color |
| --slide-split-padding-top | 16px | Padding top of a slide split pane |
| --slide-split-padding-end | 32px | Padding right of a slide split pane |
| --slide-split-padding-bottom | 16px | Padding bottom of a slide split pane |
| --slide-split-padding-start | 32px | Padding left of a slide split pane |
| --slide-split-title-padding-top | 16px | Padding top of the title of the |
| --slide-split-title-padding-end | 32px | Padding right of the title of the |
| --slide-split-title-padding-bottom | 16px | Padding bottom of the title of the |
| --slide-split-title-padding-start | 32px | Padding left of the title of the |
| --slide-padding-start | 32px | Modify slotted ul and ol padding-inline-start |
| --slide-split-align | inherit | Modify for example to center if you want to align the content in the middle |
| --slide-split-text-align | inherit | Modify for example to center if you want to align the text in the middle |
| --slide-split-title-display | none | The `slot` title is per default hidden even if you provide it. If you wish to displays it, modify this attribute |
| --zIndex | 1 | The z-index of the slide |

[DeckDeckGo]: https://deckdeckgo.com