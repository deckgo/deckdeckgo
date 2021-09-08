# deckgo-slide-poll

Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.

Add a slide "Poll" to your presentation.

## Nota bene

This template does **not** currently save the results of the voting. Each time you will refresh or launch your presentation, the poll start again.

If you would have this requirement, let us now with a new [feature request](https://github.com/deckgo/deckdeckgo/issues) in our GitHub issue tracker.

## Installation

This template could be added to your presentation using the following methods.

> This template is included per default in our Developer Kit

### From a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use this template from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script type="module" src="https://unpkg.com/@deckdeckgo/slide-poll@latest/dist/deckdeckgo-slide-poll/deckdeckgo-slide-poll.esm.js"></script>
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

<!-- Auto Generated Below -->


## Properties

| Property            | Attribute             | Description                                                                                                                                                    | Type      | Default                             |
| ------------------- | --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------- | ----------------------------------- |
| `connectPollSocket` | `connect-poll-socket` | In case you would not like that the template try to reach the socket server                                                                                    | `boolean` | `true`                              |
| `customActions`     | `custom-actions`      | If you provide actions for the all deck but, a specific one for this slide, set this option to true                                                            | `boolean` | `false`                             |
| `customBackground`  | `custom-background`   | If you define a background for the all deck but, a specific one for this slide, set this option to true                                                        | `boolean` | `false`                             |
| `pollKey`           | `poll-key`            | Per default the template will always try to create a new poll but if you set this value, it will try to retrieve an existing poll                              | `string`  | `undefined`                         |
| `pollLink`          | `poll-link`           | The url which leads to the voting application respectively where your audience will be available to make their voice heard aka where they will be able to vote | `string`  | `'https://app.deckdeckgo.com/poll'` |
| `socketPath`        | `socket-path`         | The path to reach the socket server                                                                                                                            | `string`  | `'/poll'`                           |
| `socketUrl`         | `socket-url`          | The url of the socket (server) where the poll (chat room) is going to be created                                                                               | `string`  | `'https://api.deckdeckgo.com'`      |


## Events

| Event          | Description                        | Type                |
| -------------- | ---------------------------------- | ------------------- |
| `pollUpdated`  |                                    | `CustomEvent<void>` |
| `slideDidLoad` | Triggered when the slide is loaded | `CustomEvent<void>` |


## Methods

### `afterSwipe() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `beforeSwipe(_enter: boolean, _reveal: boolean) => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `hideContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `isAnswered() => Promise<boolean>`



#### Returns

Type: `Promise<boolean>`



### `lazyLoadContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `resizeContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `revealContent() => Promise<void>`



#### Returns

Type: `Promise<void>`



### `update() => Promise<void>`



#### Returns

Type: `Promise<void>`




## Slots

| Slot               | Description                                                                                                                                                                                                                                       |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `"actions"`        | Custom actions for this slide                                                                                                                                                                                                                     |
| `"answer-x"`       | X possible answers to the question                                                                                                                                                                                                                |
| `"awaiting-votes"` | A message displayed until first vote                                                                                                                                                                                                              |
| `"background"`     | A custom background for this slide                                                                                                                                                                                                                |
| `"footer"`         | A custom footer for this slide                                                                                                                                                                                                                    |
| `"header"`         | A custom header for this slide                                                                                                                                                                                                                    |
| `"how-to"`         | Explain how can attendees vote. Note also that if you provide a string 0 in the content of your slot how-to, the information will be automatically converted to the real key of your poll (the key your audience could use to reach it and vote). |
| `"notes"`          | Some notes related to this slide                                                                                                                                                                                                                  |
| `"question"`       | A question                                                                                                                                                                                                                                        |


## CSS Custom Properties

| Name                                        | Description                                                                  |
| ------------------------------------------- | ---------------------------------------------------------------------------- |
| `--background`                              | background                                                                   |
| `--color`                                   | color                                                                        |
| `--overflow`                                | overflow of the slide @default hidden                                        |
| `--slide-padding-bottom`                    | Padding bottom of the slide @default 64px and 32px on wider screen           |
| `--slide-padding-end`                       | Padding right of the slide @default 64px and 32px on wider screen            |
| `--slide-padding-start`                     | Padding left of the slide @default 64px and 32px on wider screen             |
| `--slide-padding-top`                       | Padding top of the slide @default 64px and 32px on wider screen              |
| `--slide-poll-align-items`                  | The QR code column content items alignment default center                    |
| `--slide-poll-awaiting-votes-background`    | The background of the "awaiting-votes" slot default rgba(255, 255, 255, 0.9) |
| `--slide-poll-awaiting-votes-border-radius` | The border-radios of the "awaiting-votes" slot default 8px                   |
| `--slide-poll-awaiting-votes-padding`       | The padding of the "awaiting-votes" slot default 8px                         |
| `--slide-poll-awaiting-votes-z-index`       | The z-index of the "awaiting-votes" slot default 1                           |
| `--slide-poll-background`                   | The background behind the QR code component                                  |
| `--slide-poll-grid-column-gap`              | The column gap between the QR code and the chart default 32px                |
| `--slide-poll-how-to-font-size`             | The font-size of the text of thee slot "how-to" default 0.8em                |
| `--slide-poll-how-to-max-width`             | The maximal width of the "how-to" slot default calc(100% - 64px)             |
| `--slide-poll-justify-content`              | The QR code column content justify position default center                   |
| `--slide-poll-text-align`                   | The QR code column text alignment default center                             |
| `--slide-user-select`                       | user select @default none                                                    |
| `--zIndex`                                  | z-index @default 1                                                           |


----------------------------------------------

*Built with [StencilJS](https://stenciljs.com/)*
