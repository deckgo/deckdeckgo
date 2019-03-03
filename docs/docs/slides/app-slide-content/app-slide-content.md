# Slide: Content

The "Content" slide is a simple slide which display its title and content aligned to the start of the page.

This slide could be for example use for the every slides of your presentation where you would like to display content related to your talk.

## Table of contents

- [Layout](#app-slide-content-layout)
- [Usage](#app-slide-content-usage)
  - [Slots](#app-slide-content-slots)
  - [Notes](#app-slide-content-notes)
- [Attributes](#app-slide-content-attributes)
  - [Example](#app-slide-content-example)
- [Theming](#app-slide-content-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-content>
        <h1 slot="title">Something related to my topic</h1>
        <p slot="content">
          Cool beans
        </p>
      </deckgo-slide-content>
  </deckgo-deck>
</div>

## Usage

The "Content" slide's Web Component could be integrated using the tag `<deckgo-slide-content/>`.

```
<deckgo-deck>
  <deckgo-slide-content>
    <h1 slot="title">Something related to my topic</h1>
    <p slot="content">
      Cool beans
    </p>
  </deckgo-slide-content>
</deckgo-deck>
```

### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

### Example

```
<deckgo-deck>
  <deckgo-slide-content reveal="true" reveal-show-first="true">
    <h1 slot="title">Something related to my topic</h1>
    <ul slot="content">
      <li>Cool</li>
      <li>Beans</li>
    </ul>
  </deckgo-slide-content>
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