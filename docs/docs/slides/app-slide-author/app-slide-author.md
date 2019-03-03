# Slide: Author

The "Author" slide let you introduce the author of the presentation.

## Table of contents

- [Layout](#app-slide-author-layout)
- [Usage](#app-slide-author-usage)
  - [Slots](#app-slide-author-slots)
  - [Notes](#app-slide-author-notes)
  - [Social components](#app-slide-author-social-components)
- [Attributes](#app-slide-author-attributes)
  - [Example](#app-slide-author-example)
- [Theming](#app-slide-author-theming)

## Layout

<div class="container" margin>
  <deckgo-deck embedded={true} pager={false}>
    <deckgo-slide-author img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social twitter="daviddalbusco"><ion-icon area-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon> twitter</deckgo-social></div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social linkedin="david-dal-busco/"><ion-icon area-label="David on Linkedin" slot="icon" name="logo-linkedin"></ion-icon> linkedin</deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>

## Usage

The "Author" slide's Web Component could be integrated using the tag `<deckgo-slide-author/>`.

```
<deckgo-deck>
  <deckgo-slide-author img-src="/assets/author.jpeg" img-alt="My self">
    <h1 slot="title">Author</h1>
    <div slot="author">
      <h2>David</h2>
      <p>Something about me</p>
    </div>
    <div slot="social-link"><deckgo-social twitter="daviddalbusco">twitter</deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>  
```

### Slots

Both slots `title`, `author` and `social-link` are optional, but of course the slide would looks better with at least the slot `author` would be provided.

Notes: 

* The slot `title` is hidden. If you use the [DeckDeckGo] starter, it will be use for the navigation modal

* You could provide up to six `social-link` slots. Each of these could be your custom code or you could use the component `<deckgo-social/>` to easily provide a link to an external URI.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute `show`.

### Social components

The details of the component `<deckgo-social/>` is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| img-src | string |  | An image URI, for example a picture of the author. Note: this image will be displayed as a circle. |
| img-alt | string |  | An optional accessibility alt for the image. |

### Example

```
<deckgo-deck>
  <deckgo-slide-author img-src="/assets/author.jpeg">
    <div slot="author">
      <h2>David</h2>
      <p>Something about me</p>
    </div>
    <div slot="social-link"><deckgo-social twitter="daviddalbusco">twitter</deckgo-social></div>
    <div slot="social-link"><deckgo-social linkedin="david-dal-busco/">linkedin</deckgo-social></div>
    <div slot="social-link"><deckgo-social medium="david.dalbusco">medium</deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note |
| -------------------------- |-----------------|-----------------|
| --background |  |  |
| --color |  |  |
| --slide-author-background-start | | Left pane background |
| --slide-author-color-start | | Left pane color |
| --slide-author-background-end | | Right pane background |
| --slide-author-color-end | | Right pane color |
| --slide-author-padding-top | 16px | Padding top of a slide |
| --slide-author-padding-end | 32px | Padding right of a slide |
| --slide-author-padding-bottom | 16px | Padding bottom of a slide |
| --slide-author-padding-start | 32px | Padding left of a slide |
| --slide-padding-start | 32px | Modify slotted ul and ol padding-inline-start |
| --slide-author-align | inherit | Modify for example to center if you want to align the content in the middle |
| --slide-author-text-align | inherit | Modify for example to center if you want to align the text in the middle |
| --slide-author-img-size | 80% | The size of the image of the left pane |
| --slide-author-social-padding-top | 32px | The spacing between the author description and the social links |
| --zIndex | 1 | The z-index of the slide |

[DeckDeckGo]: https://deckdeckgo.com