[![npm][npm-badge]][npm-badge-url]
[![license][npm-license]][npm-license-url]

[npm-badge]: https://img.shields.io/npm/v/@deckdeckgo/remote
[npm-badge-url]: https://www.npmjs.com/package/@deckdeckgo/remote
[npm-license]: https://img.shields.io/npm/l/@deckdeckgo/remote
[npm-license-url]: https://github.com/deckgo/deckdeckgo/blob/master/webcomponents/remote/LICENSE

# DeckDeckGo - Remote

This project is a Web Component which act as the remote control receiver of the [Progressive Web App](https://deckdeckgo.app) of [DeckDeckGo] .

## Table of contents

- [Features](#features)
- [Getting Started](#getting-started)
  - [Standalone](#standalone)
  - [Install from NPM](#install-from-npm)
  - [Using from a CDN](#using-from-a-cdn)
  - [Framework integration](#framework-integration)
- [Usage](#usage)
  - [Attributes](#attributes)
  - [Methods](#methods)
- [License](#license)

## Features

Once added in your project it will add the two following two main features:

1. It create a real-time communication channel using [WebRTC](https://webrtc.org) with the [Progressive Web App](https://deckdeckgo.app) of [DeckDeckGo]
2. It add a canvas over the content which will reflect what will be drawn in the [Progressive Web App](https://deckdeckgo.app)

## Getting Started

Start you new presentation by following the quick 👉 [Getting Started guide](https://docs.deckdeckgo.com/docs) 👈

### Standalone

To create easily your PWA presentation and to enjoy all the options, I suggest you to use the starter kit as described in the [documentation](https://docs.deckdeckgo.com/docs).

However, if you wish to use this Web Component as a standalone component to add a remote control to your DeckDeckGo's presentation you would have installed as standalone too, install it as the following:

### Install from NPM

Install it in your project from [npm](https://www.npmjs.com/package/deckdeckgo) using the following command:

```bash
npm install deckdeckgo-remote
```

### Using from a CDN

It's recommended to use [unpkg](https://unpkg.com/) to use this component from a CDN. To do so, add the following include script in the main HTML file of your project:

```
<script src="https://unpkg.com/deckdeckgo-remote@latest/dist/deckdeckgo-remote.js"></script>
```

### Framework integration

The [Stencil documentation](https://stenciljs.com/docs/overview) provide examples of framework integration for [Angular](https://stenciljs.com/docs/angular), [React](https://stenciljs.com/docs/react), [Vue](https://stenciljs.com/docs/vue) and [Ember](https://stenciljs.com/docs/ember).

## Usage

This Web Component could be integrated and used like the following:

```
<deckgo-remote room="my_presentation" width="300" height="200" slides="5"></deckgo-remote>
```

For a more concrete example you could have a look to the [DeckDeckGo Starter-kit](https://github.com/deckgo/deckdeckgo-starter)

### Attributes

The following options should be set using attributes:

| Attribute   |   Type   |                                                                     Description                                                                      |
| ----------- | :------: | :--------------------------------------------------------------------------------------------------------------------------------------------------: |
| room        |  string  | The room respectively the presentation's name which will be displayed to the user in the app when he/she will search for presentations to connect to |
| socketUrl   |  string  |                                         A socket URI, in case you would implement your own signaling server                                          |
| width       |  number  |                                                               The width of the canvas                                                                |
| height      |  number  |                                                               The height of the canvas                                                               |
| length      |  number  |                How many slides contains your deck, this is useful to inform the app about it and initialize the length of the canvas                 |
| slides      | string[] |      The ordered list of all the tag names of the slides. Useful if you would like to inform the app about which slides are used in your deck.       |
| autoConnect | boolean  |                                   If you the component to not connect itself on load, set this property to `false`                                   |

### Methods

Furthermore to attributes, this Web Component exposes methods too.

**Note:** For description purpose, we reference a `remote` object we would have been initialized like the following in Vanilla Javascript:

```
const remote = document.getElementsByTagName('deckgo-remote');
```

#### Connect

Connect will open a socket with the signaling server and create a room for the presentation. If an application would wait for such a room, it will also inform this application that the receiver is yet available in order to establish the WebRTC connection.

```
await remote.connect();
```

#### Disconnect

Disconnect will close the socket with the signaling server and remove the deck from the list of possible deck to connect to.

```
await remote.disconnect();
```

#### Move canvas

You might need to move the canvas, this method let's you move it.

```
await remote.moveDraw(leftOffset, transitionDuration);
```

#### Next slide

If you would slide in your presentation, you could call this method in order to send the information to the application, in order to reflect the slide change in the application too.

```
await remote.nextSlide();
```

#### Prev slide

If you would slide in your presentation, you could call this method in order to send the information to the application, in order to reflect the slide change in the application too.

```
await remote.prevSlide();
```

### Go to a specific slide

This method will emit an event from the deck to slide to a specific slide.

```
await remote.slideTo(0); // parameters: index: number, speed?: number | undefined
```

### Update slides

This method will emit an event from the deck to the remote in order to update the list of slides.

```
await remote.updateSlides();
```

### Delete slide

This method will delete the current slide in the remote app.

```
await remote.deleteSlide();
```

### Update slide

This method will emit an event to update a particular slide in the remote.

```
await remote.updateSlides(index, slideDefinition);
```

### Update reveal

This method will emit an event to update the reveal settings of the deck of the remote.

```
await remote.updateReveal(reveal);
```

### Play

This method will tell the remote app that the play action was performed in the deck.

```
await remote.play();
```

### Pause

This method will tell the remote app that the pause action was performed in the deck.

```
await remote.pause();
```

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[deckdeckgo]: https://deckdeckgo.com
