# YouTube

The "YouTube" component allows you to easily add a [YouTube](https://youtube.com) video in almost any slide of your presentation.

## Table of contents

- [Showcase](#app-components-youtube-showcase)
- [Installation](#app-components-youtube-installation) - [Using from a CDN](#app-components-youtube-from-a-cdn) - [Install from NPM](#app-components-youtube-from-npm) - [Framework integration](#app-components-youtube-framework-integration)
- [Usage](#app-components-youtube-usage)
  - [Slots](#app-components-youtube-slots)
  - [Attributes](#app-components-youtube-attributes)
  - [Methods](#app-components-youtube-methods)
    - [Lazy load the video](#app-components-youtube-lazy-load-the-video)
    - [Modify video size on the fly](#app-components-youtube-modify-video-size-on-the-fly)
    - [Play the video](#app-components-youtube-play-the-video)
    - [Pause the video](#app-components-youtube-pause-the-video)

## Showcase

<div>
  <deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw" width={300} height={200}>
  </deckgo-youtube>
</div>

## Installation

This component could be added to your web application using the following methods.

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use the [DeckDeckGo] lazy image component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/youtube@latest/dist/deckdeckgo-youtube/deckdeckgo-youtube.esm.js"></script>
```

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/qrcode) using the following command:

```bash
npm install @deckdeckgo/youtube
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/youtube';
```

#### Loader

```
import { defineCustomElements as deckDeckGoElement } from '@deckdeckgo/youtube/dist/loader';
deckDeckGoElement();
```

## Usage

The "YouTube" slide's Web Component could be integrated using the tag `<deckgo-youtube/>`.

```
<deckgo-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
</deckgo-youtube>
```

### Slots

No slots are available for this component.

### Attributes

This component offers the following options which could be set using attributes:

| Attribute   | Type    | Default | Description                                                                                                                               |
| ----------- | ------- | ------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| src         | string  |         | The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube. |
| width       | number  |         | The width of the video player.                                                                                                            |
| height      | number  |         | The height of the video player.                                                                                                           |
| frame-title | string  |         | A title for the frame, could be use for accessibility reason.                                                                             |
| instant     | boolean | false   | In case you would like to load the video as soon as the component is loaded.                                                              |

Per default the video, respectively its `iframe`, won't be loaded (expect if you specify `instant` to `true`). Therefore it's up to you to call the method `lazyLoadContent` to create the video. The reason behind this decision is allowing you to lazy load your content.

### Methods

The `<deckgo-youtube/>` component exposes the following methods:

#### Lazy load the video

```
lazyLoadContent(): Promise<void>
```

#### Modify video size on the fly

```
async updateIFrame(width: number, height: number)
```

#### Play the video

```
play(): Promise<void>
```

#### Pause the video

```
pause(): Promise<void>
```

### Examples

The following code:

```
<deckgo-youtube width="200" height="100" instant="true" src="https://www.youtube.com/embed/Y97mEj9ZYmE" frameTitle="DeckDeckGo editor demo"></deckgo-youtube>
```

Renders the following video:

<div class="container ion-margin">
    <deckgo-youtube width={200} height={100} instant={true} src="https://www.youtube.com/embed/Y97mEj9ZYmE" frameTitle="DeckDeckGo editor demo"></deckgo-youtube>
</div>

[deckdeckgo]: https://deckdeckgo.com
