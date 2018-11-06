# DeckDeckGo - Slides

[DeckDeckGo] is a deck of slides where each slide has its own layout and behaviour. Their content could be edited and structured using the provided `slots`.

Some templates offer extra features as for example the slide [Code](#slide-code) which optionally offer the ability to zoom in the displayed code.

## Table of contents

- [Slide: Title](#slide-title)
- [Slide: Content](#slide-content)
- [Slide: Split](#slide-split)
- [Slide: Code](#slide-code)

## Slide: Title

The "Title" slide is a simple slide which display its title and content center in the middle of the page.

This slide could be for example use for the very first and last slide of your presentation.

### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-title-layout.png" width="450px">

### Usage

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

#### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

#### Example

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

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |

## Slide: Content

The "Content" slide is a simple slide which display its title and content aligned to the start of the page.

This slide could be for example use for the every slides of your presentation where you would like to display content related to your talk.

### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-content-layout.png" width="450px">

### Usage

The "Content" slide's Web Component could be integrated using the tag `<deckgo-slide-content/>`.

```
<deckgo-deck>
  <deckgo-slide-content>
    <h1 slot="title">Something related to my topic</h1>
    <p slot="content">
      Cool beans
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

#### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

#### Example

```
<deckgo-deck>
  <deckgo-slide-content reveal="true" reveal-show-first="true">
    <h1 slot="title">Something related to my topic</h1>
    <ul slot="content">
      <li>Cool</li>
      <li>Beans</li>
    </p>
  </deckgo-slide-title>
</deckgo-deck>
```

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |

## Slide: Split

The "Split" slide is a simple slide which display two panes on the page.

### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-split-layout.png" width="450px">

### Usage

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
  </deckgo-slide-title>
</deckgo-deck>
```

#### Slots

Both slots `title` and `content` are optional. Without providing one of them, the page will remain empty.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| reveal | boolean | false | Hide the slotted elements `li`, `p` an `img` and display them when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| reveal-show-first | boolean | false | Show the first elements which would be hidden if `reveal` is set to `true` |

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
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

## Slide: Code

The "Code" slide is a the slide to use if you would like to showcase code during your talk.

### Layout

<img src="https://github.com/fluster/deckdeckgo/blob/master/doc/slides/deckdeckgo-slide-code-layout.gif" width="450px">

### Usage

The "Code" slide's Web Component could be integrated using the tag `<deckgo-slide-code/>`.

```
<deckgo-slide-code src-file="https://domain.com/path-to-my-code.extension">
  <h1 slot="title">My code</h1>
</deckgo-slide-code>
```

#### Slots

The slot `title` is optional.

### Attributes

The attribute `src-file` is for this component mandatory. Other attributes are optional.

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |:-----------------:|:-----------------:|:-----------------:|
| src-file | string | | The web url to the source code you would like to showcase |
| anchor | string | // DeckDeckGo | The anchor identifier which will be use to scroll through your code when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| anchor-zoom | string | // DeckDeckGoZoom | The anchor identifier which will be use to zoom inside your code when navigating using `slideNext()` or `slidePrev()` (see [documention](/doc/features/navigation.md)) |
| hide-anchor | boolean | true | Set this attribute to `false` in case you would like to actually display the anchor value too |

#### Example

```
<deckgo-slide-code hide-anchor="fals" src-file="https://raw.githubusercontent.com/fluster/deckdeckgo/master/src/components/slides/deckdeckgo-slide-code/deckdeckgo-slide-code.tsx">
  <h1 slot="title">Code</h1>
</deckgo-slide-code>
```

### Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |:-----------------:|:-----------------:|
| --background |  |  |
| --color |  |  |
| --code-color |  | The color of the displayed code |
| --code-font-size |  | The size of the font for the code |
| --code-font-family |  | The family of the font for the code |
| --slide-padding-top | 16px | Padding top of the all slide |
| --slide-padding-end | 32px | Padding right of the all slide |
| --slide-padding-bottom | 16px | Padding bottom of the all slide |
| --slide-padding-start | 32px | Padding left of the all slide |
| --code-margin-bottom | 64px | Padding bottom of the code scroller |
| --zoom-code | 1 | If you wish to manually zoom the code |

[DeckDeckGo]: https://deckdeckgo.com