# Slide: Title

The "Title" slide is a simple slide which display its title and content center in the middle of the page.

This slide could be for example use for the very first and last slide of your presentation.

## Table of contents

- [Layout](#app-slide-title-layout)
- [Usage](#app-slide-title-usage)
  - [Slots](#app-slide-title-slots)
  - [Notes](#app-slide-title-notes)
- [Attributes](#app-slide-title-attributes)
- [Example](#app-slide-title-example)
- [Theming](#app-slide-title-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-title>
      <h1 slot="title">My presentation title</h1>
      <p slot="content">
        Hello World 🚀
      </p>
    </deckgo-slide-title>
  </deckgo-deck>
</div>

## Usage

The "Title" slide's Web Component could be integrated using the tag `<deckgo-slide-title/>`.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <p slot="content">
      Hello World 🚀
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <div slot="notes">A note regarding this particular slide</div>
    
And another note on a new line about it too.
  </deckgo-slide-title>
</deckgo-deck>
```

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

```
<deckgo-deck>
  <deckgo-slide-title>
    <h1 slot="title">My presentation title</h1>
    <div slot="notes" show>A note displayed in the presentation within a modal accessible for anyone</div>
  </deckgo-slide-title>
</deckgo-deck>
```

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

## Example

```
<deckgo-deck>
  <deckgo-slide-title reveal="true" reveal-show-first="true">
    <h1 slot="title">My presentation title</h1>
    <ul slot="content">
      <li>Hello</li>
      <li>World</li>
      <li>🚀</li>
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --zIndex | 1 | The z-index of the slide |

[DeckDeckGo]: https://deckdeckgo.com