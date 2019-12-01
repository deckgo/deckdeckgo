# Slide: Youtube

The "Youtube" slide let you add easily a [Youtube](https://youtube.com) video to your presentation.

## Table of contents

- [Layout](#app-slide-youtube-layout)
- [Installation](#app-slide-youtube-installation)
  - [From a CDN](#app-slide-youtube-from-a-cdn)
  - [From NPM](#app-slide-youtube-from-npm)
  - [Framework integration](#app-slide-youtube-framework-integration)
- [Usage](#app-slide-youtube-usage)
  - [Slots](#app-slide-youtube-slots)
  - [Notes](#app-slide-youtube-notes)
  - [Youtube component](#app-slide-youtube-youtube-component)
- [Attributes](#app-slide-youtube-attributes)
- [Theming](#app-slide-youtube-theming)
- [Methods](#app-slide-youtube-methods)
  - [Play the video](#app-slide-youtube-play-the-video)
  - [Pause the video](#app-slide-youtube-pause-the-video)
  - [Toggle the video](#app-slide-youtube-toggle-the-video)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
      <h1 slot="title">A 16/9 video</h1>
    </deckgo-slide-youtube>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.
 
### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-youtube@latest/dist/deckdeckgo-slide-youtube/deckdeckgo-slide-youtube.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/slide-youtube@latest/dist/deckdeckgo-slide-youtube/deckdeckgo-slide-youtube.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/core) run the following command:

```bash
npm install @deckdeckgo/slide-youtube
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-youtube';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-youtube/dist/loader';
deckDeckGoSlideElement(window);
```

## Usage

The "Youtube" slide's Web Component could be integrated using the tag `<deckgo-slide-youtube/>`.

```
<deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
  <h1 slot="title">A 16/9 video</h1>
</deckgo-slide-youtube>
```

### Slots

The slot `title` and `content` are optional. The slot `content` is displayed before the video.

### Notes

Optionally a slot `notes` could be use to add some notes regarding the particular slide. These will be automatically `displayed` in the [remote control](https://deckdeckgo.app).

If you are using the [DeckDeckGo] starter kit and wish to make your notes accessible to anyone, you will need to mark them with the attribute `show`.

### Youtube component

The slide "Youtube" relies on the component `<deckgo-youtube/>` which is described in the components [documentation](https://github.com/deckgo/deckdeckgo/blob/master/doc/components/components.md).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute                      | Type   | Default   | Description   |
| -------------------------- |-----------------|-----------------|-----------------|
| src | string |  | The source url, the Youtube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by Youtube. |
| width | number | Per default the video width will be calculated according the content size available. | Using this option you would be able to define your own width. |
| height | number | Per default the video height will be calculated according the content size available. | Using this option you would be able to define your own height. |
| custom-background | boolean | false | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions | boolean | false | If you would provide actions for the all deck and a specific one for this slide, set this option to `true` |

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
| --slide-youtube-margin | 0 0 32px | The margin of the video's container |
| --slide-youtube-height | calc(100% - 32px) | The height of the video's container |
| --slide-youtube-overflow | auto | The overflow of the video's container |

## Methods

The slide "Youtube" offers extra methods to play and pause the Youtube video clip. These methods are notably used by the [DeckDecGo]'s remote control.

### Play the video

```
const slide = deck.getElementsByTagName('deckgo-slide-youtube');
await slide.play();
```

### Pause the video

```
const slide = deck.getElementsByTagName('deckgo-slide-youtube');
await slide.pause();
```

### Toggle the video

Toggle will take care to pause or play the video according its current state.

```
const slide = deck.getElementsByTagName('deckgo-slide-youtube');
await slide.toggle();
```

[DeckDeckGo]: https://deckdeckgo.com