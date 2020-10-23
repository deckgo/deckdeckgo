# Slide: Author

The "Author" slide lets you introduce the author of the presentation.

## Table of contents

- [Layout](#app-slide-author-layout)
- [Installation](#app-slide-author-installation)
  - [From a CDN](#app-slide-author-from-a-cdn)
  - [From NPM](#app-slide-author-from-npm)
  - [Framework integration](#app-slide-author-framework-integration)
  - [Social component](#app-slide-author-social-component)
- [Usage](#app-slide-author-usage)
  - [Slots](#app-slide-author-slots)
  - [Social components](#app-slide-author-social-components)
- [Attributes](#app-slide-author-attributes)
  - [Example](#app-slide-author-example)
- [Theming](#app-slide-author-theming)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-author img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
        <h1 slot="title">slot="title"</h1>
        <div slot="author">slot="author"</div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social twitter="daviddalbusco"><ion-icon aria-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon></deckgo-social></div>
        <div slot="social-link" style={{fontSize: '0.5rem'}}><deckgo-social linkedin="david-dal-busco"><ion-icon aria-label="David on LinkedIn" slot="icon" name="logo-linkedin"></ion-icon></deckgo-social></div>
    </deckgo-slide-author>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit this template is included. You don't need to install it so therefore you should skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-author@latest/dist/deckdeckgo-slide-author/deckdeckgo-slide-author.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-author) run the following command:

```bash
npm install @deckdeckgo/slide-author
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-author';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-author/dist/loader';
deckDeckGoSlideElement();
```

### Social component

This templates relies on the `@deckdeckgo/social` component without any explicit dependency. Therefore it should also be installed, see its related [installation](/components/social) guide.

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
    <div slot="social-link"><deckgo-social twitter="daviddalbusco"></deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>
```

### Slots

Slots for `title`, `author` and `social-link` are optional. It is recommended that the slot `author` be filled as to improve the appearance of the slide.

Notes:

- The slot `title` is hidden. If you use the [DeckDeckGo] starter, it will be used for the navigation model.

- You could provide up to six `social-link` slots. Each of these could be your custom code or you could use the component `<deckgo-social/>` to easily provide a link to an external URI.

### Social components

The details of the component `<deckgo-social/>` is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type                            | Default   | Description                                                                                                                                                                 |
| ----------------- | ------------------------------- | --------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| img-src           | string                          |           | An image URI, for example a picture of the author. Note: this image will be displayed as a circle.                                                                          |
| img-alt           | string                          |           | An optional accessibility alt for the image.                                                                                                                                |
| mode              | `"circle" \| "cover" \| "none"` | `'cover'` | The design to be applied to the image. `cover` fits the image to the start pane, `circle` displays it in a circle and `none` in case you would not like to display an image |
| custom-background | boolean                         | false     | If you will provide a background for the all deck and a specific one for this slide, set this option to `true`                                                              |
| custom-actions    | boolean                         | false     | If you will provide actions for the all deck and a specific one for this slide, set this option to `true`                                                                   |

### Example

```
<deckgo-deck>
  <deckgo-slide-author img-src="/assets/author.jpeg">
    <div slot="author">
      <h2>David</h2>
      <p>Something about me</p>
    </div>
    <div slot="social-link"><deckgo-social twitter="daviddalbusco"></deckgo-social></div>
    <div slot="social-link"><deckgo-social linkedin="david-dal-busco"></deckgo-social></div>
    <div slot="social-link"><deckgo-social medium="david.dalbusco"></deckgo-social></div>
  </deckgo-slide-author>
</deckgo-deck>
```

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                      | Default | Note                                                                                |
| ---------------------------------- | ------- | ----------------------------------------------------------------------------------- |
| --background                       |         |                                                                                     |
| --color                            |         |                                                                                     |
| --slide-author-background-start    |         | Left pane background                                                                |
| --slide-author-color-start         |         | Left pane color                                                                     |
| --slide-author-background-end      |         | Right pane background                                                               |
| --slide-author-color-end           |         | Right pane color                                                                    |
| --slide-author-padding-top         | 16px    | Padding top of a slide                                                              |
| --slide-author-padding-end         | 32px    | Padding right of a slide                                                            |
| --slide-author-padding-bottom      | 16px    | Padding bottom of a slide                                                           |
| --slide-author-padding-start       | 32px    | Padding left of a slide                                                             |
| --slide-padding-start              | 32px    | Modify slotted ul and ol padding-inline-start                                       |
| --slide-author-align               | inherit | Modify for example to center if you want to align the content in the middle         |
| --slide-author-text-align          | inherit | Modify for example to center if you want to align the text in the middle            |
| --slide-author-img-size            | 80%     | The size of the image of the left pane                                              |
| --slide-author-img-border          |         | The border of the image of the left pane (only apply if `circle` mode is specified) |
| --slide-author-social-padding-top  | 32px    | The spacing between the author description and the social links                     |
| --zIndex                           | 1       | The z-index of the slide                                                            |
| --slide-author-social-link-padding | 8px     | Padding for the social links                                                        |

[deckdeckgo]: https://deckdeckgo.com
