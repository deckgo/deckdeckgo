# Slide: Poll

Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.

Add a slide "Poll" to your presentation.

## Table of contents

- [Layout](#app-slide-poll-layout)
- [Nota bene](#app-slide-poll-nota-bene)
- [Installation](#app-slide-poll-installation)
  - [From a CDN](#app-slide-poll-from-a-cdn)
  - [From NPM](#app-slide-poll-from-npm)
  - [Framework integration](#app-slide-poll-framework-integration)
- [Usage](#app-slide-poll-usage)
  - [Slots](#app-slide-poll-slots)
  - [YouTube component](#app-slide-poll-youtube-component)
- [Attributes](#app-slide-poll-attributes)
- [Theming](#app-slide-poll-theming)
- [Methods](#app-slide-poll-methods)
  - [Play the video](#app-slide-poll-play-the-video)
  - [Pause the video](#app-slide-poll-pause-the-video)
  - [Toggle the video](#app-slide-poll-toggle-the-video)
  - [Get the video](#app-slide-poll-get-the-video)

## Layout

<div class="container ion-margin">
  <deckgo-deck embedded={true}>
    <deckgo-slide-poll poll-link="https://deckdeckgo.com/poll" socket-url="https://api.deckdeckgo.com">
        <h1 slot="question">Do you like my presentation so far?</h1>
        <p slot="answer-1">It is super</p>
        <p slot="answer-2">Meh</p>
        <p slot="answer-3">I could'nt care less</p>
        <p slot="answer-4">Tell me why</p>
        <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> and use the code {0}</p>
        <p slot="awaiting-votes">Awaiting first votes</p>
        <p slot="answer-5">Ain't nothin' but a heartache</p>
      </deckgo-slide-poll>
  </deckgo-deck>
</div>

## Nota bene

This template does **not** currently save the results of the voting. Each time you will refresh or launch your presentation, the poll start again.

If you would have this requirement, let us now with a new [feature request](https://github.com/deckgo/deckdeckgo/issues) in our GitHub issue tracker.

## Installation

This template could be added to your presentation using the following methods.

> If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the "Installation" chapter.

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-poll@latest/dist/deckdeckgo-slide-poll/deckdeckgo-slide-poll.esm.js"></script>
<script nomodule="" src="https://unpkg.com/@deckdeckgo/slide-poll@latest/dist/deckdeckgo-slide-poll/deckdeckgo-slide-poll.js"></script>
```

### From NPM

To install this template in your project from [npm](https://www.npmjs.com/package/@deckdeckgo/slide-poll) run the following command:

```bash
npm install @deckdeckgo/slide-poll
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

That being said, commonly, you might either `import` or `load` it:

#### Import

```
import '@deckdeckgo/slide-poll';
```

#### Loader

```
import { defineCustomElements as deckDeckGoSlideElement } from '@deckdeckgo/slide-poll/dist/loader';
deckDeckGoSlideElement();
```

## Usage

The "Poll" slide's Web Component could be integrated using the tag `<deckgo-slide-poll/>`.

```
<deckgo-slide-poll poll-link="https://deckdeckgo.com" socket-url="https://api.deckdeckgo.com">
    <h1 slot="question">Do you like my presentation so far?</h1>
    <p slot="answer-1">It is super</p>
    <p slot="answer-2">Meh</p>
    <p slot="answer-3">I could'nt care less</p>
    <p slot="answer-4">Tell me why</p>
    <p slot="answer-5">Ain't nothin' but a heartache</p>
    <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> and use the code {0}</p>
    <p slot="awaiting-votes">Awaiting first votes</p>
</deckgo-slide-poll>
```

### Slots

The slots `question` and at least one `answer` should be provided. Answer slots have to be provided as `answer-x` where `x` is a number bigger than 0.

The slot `how-to` and `awaiting-votes` are optional, still, it's probably for best of your audience to provide these.

Note also that if you provide a string `{0}` in the content of your slot `how-to`, the information will be automatically converted to the real key of your poll (the key your audience could use to reach it and vote).

## Attributes

This component offers the following options which could be set using attributes:

| Attribute         | Type    | Default                       | Description                                                                                                                                                     |
| ----------------- | ------- | ----------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| socketUrl         | string  | `https://api.deckdeckgo.com`  | The url of the socket (server) where the poll (chat room) is going to be created.                                                                               |
| socketPath        | string  | `/poll`                       | The path to reach the socket server                                                                                                                             |
| connectPollSocket | boolean | `true`                        | In case you would not like that the template try to reach the socket server                                                                                     |
| pollLink          | string  | `https://deckdeckgo.com/poll` | The url which leads to the voting application respectively where your audience will be available to make their voice heard aka where they will be able to vote. |
| pollKey           | string  |                               | Per default the template will always try to create a new poll but if you set this value, it will try to retrieve an existing poll                               |

## Theming

The following theming options will affect this component if set on its host or parent.

| CSS4 variable                             | Default               | Note                                             |
| ----------------------------------------- | --------------------- | ------------------------------------------------ |
| --background                              |                       |                                                  |
| --color                                   |                       |                                                  |
| --slide-padding-top                       | 16px                  | Padding top of the all slide                     |
| --slide-padding-end                       | 32px                  | Padding right of the all slide                   |
| --slide-padding-bottom                    | 16px                  | Padding bottom of the all slide                  |
| --slide-padding-start                     | 32px                  | Padding left of the all slide                    |
| --slide-poll-grid-column-gap              | 32px                  | The column gap between the QR code and the chart |
| --slide-poll-justify-content              | center                | The QR code column content justify position      |
| --slide-poll-align-items                  | center                | The QR code column content items alignment       |
| --slide-poll-text-align                   | center                | The QR code column text alignment                |
| --slide-poll-background                   |                       | The background behind the QR code component      |
| --slide-poll-how-to-max-width             | calc(100% - 64px)     | The maximal width of the "how-to" slot           |
| --slide-poll-how-to-font-size             | 0.8em                 | The font-size of the text of thee slot "how-to"  |
| --slide-poll-awaiting-votes-z-index       | 1                     | The z-index of the "awaiting-votes" slot         |
| --slide-poll-awaiting-votes-background    | rgba(255,255,255,0.9) | The background of the "awaiting-votes" slot      |
| --slide-poll-awaiting-votes-border-radius | 8px                   | The border-radios of the "awaiting-votes" slot   |
| --slide-poll-awaiting-votes-padding       | 8px                   | The padding of the "awaiting-votes" slot         |

Moreover, this component is using the Web Components "QR Code" and "Bar chart". Their respective CSS4 variables could be applied too.

## Methods

The slide "Poll" offers some extra methods.

### Update

Update not in progress, update the answers and chart of the poll.

```
const slide = deck.getElementsByTagName('deckgo-slide-poll');
await slide.update();
```

### Is answered

Test if the poll has been at least answered once by a member of your audience.

```
const slide = deck.getElementsByTagName('deckgo-slide-poll');
await slide.isAnswered(); // resolve a boolean
```

[deckdeckgo]: https://deckdeckgo.com
