# Slide: Video

The "Video" slide let you add your own video or for example a GIF as MPEG-4 (MP4) to your presentation.

## Table of contents

- [Layout](#app-slide-video-layout)
- [Installation](#app-slide-video-installation)
  - [From a CDN](#app-slide-video-from-a-cdn)
  - [From NPM](#app-slide-video-from-npm)
  - [Framework integration](#app-slide-video-framework-integration)
- [Usage](#app-slide-video-usage)
  - [Slots](#app-slide-video-slots)
  - [YouTube component](#app-slide-video-youtube-component)
- [Attributes](#app-slide-video-attributes)
- [Theming](#app-slide-video-theming)
- [Methods](#app-slide-video-methods)
  - [Play the video](#app-slide-video-play-the-video)
  - [Pause the video](#app-slide-video-pause-the-video)
  - [Toggle the video](#app-slide-video-toggle-the-video)
  - [Get the video](#app-slide-video-get-the-video)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-video src="https://media.giphy.com/media/vv41HlvfogHAY/giphy.mp4">
      <h1 slot="title">A GIF as video</h1>
      <button slot="actions" onClick={() => this.playPauseVideo()}>Play/pause</button>
    </deckgo-slide-video>
  </deckgo-deck>
</div>

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) if you want to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-video@latest/dist/deckdeckgo-slide-video/deckdeckgo-slide-video.esm.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-video) run the following command:

```bash
npm install @deckdeckgo/slide-video
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-video';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-video/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Video" slide's Web Component could be integrated using the tag `<deckgo-slide-video/>`.

```
<deckgo-slide-video src="https://media.giphy.com/media/vv41HlvfogHAY/giphy.mp4">
  <h1 slot="title">A GIF as video</h1>
</deckgo-slide-video>
```

### Slots

The slot `title` and `content` are optional. The slot `content` is displayed before the video.

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default                                                                               | Description                                                                                                     |
| ----------------- | ------- | ------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| autoplay          | boolean | false                                                                                 | Automatically start the video when the slide is displayed/reached.                                              |
| loop              | boolean | false                                                                                 | Loop the video.                                                                                                 |
| muted             | boolean | true                                                                                  | Per default, the video is displayed without sounds.                                                             |
| playsinline       | boolean | true                                                                                  | Per default, the video plays inline.                                                                            |
| type              | string  | 'video/mp4'                                                                           | The type of video.                                                                                              |
| src               | string  |                                                                                       | The source of the video. Could be a video added in your `assets` or an url.                                     |
| width             | number  | Per default the video width will be calculated according the content size available.  | Using this option you would be able to define your own width.                                                   |
| height            | number  | Per default the video height will be calculated according the content size available. | Using this option you would be able to define your own height.                                                  |
| custom-background | boolean | false                                                                                 | If you would provide a background for the all deck and a specific one for this slide, set this option to `true` |
| custom-actions    | boolean | false                                                                                 | If you would provide actions for the all deck and a specific one for this slide, set this option to `true`      |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable          | Default | Note                            |
| ---------------------- | ------- | ------------------------------- |
| --background           |         |                                 |
| --color                |         |                                 |
| --slide-padding-top    | 16px    | Padding top of the all slide    |
| --slide-padding-end    | 32px    | Padding right of the all slide  |
| --slide-padding-bottom | 16px    | Padding bottom of the all slide |
| --slide-padding-start  | 32px    | Padding left of the all slide   |
| --zIndex               | 1       | The z-index of the slide        |

## Methods

The slide "Video" offers extra methods to play and pause the video clip. These methods are notably used by the [DeckDecGo]'s remote control.

### Play the video

```
const slide = deck.getElementsByTagName('deckgo-slide-video');
await slide.play();
```

### Pause the video

```
const slide = deck.getElementsByTagName('deckgo-slide-video');
await slide.pause();
```

### Toggle the video

Toggle will take care to pause or play the video according its current state.

```
const slide = deck.getElementsByTagName('deckgo-slide-video');
await slide.toggle();
```

### Get the video

The component does not expose all attributes, if you would like to interact with the video you could get a reference using the following method:

```
const slide = deck.getElementsByTagName('deckgo-slide-video');
const video = await getVideo(); // resolve an <HTMLMediaElement/> element
```

[deckdeckgo]: https://deckdeckgo.com
