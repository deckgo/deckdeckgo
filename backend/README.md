[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/deckgo/deckdeckgo/blob/master/backend/LICENSE)

# DeckDeckGo - Backend

This project is a backend for [DeckDeckGo].

It acts as the signaling server to let [DeckDeckGo]'s peers establish a real-time communication channel using [WebRTC](https://webrtc.org) for the remote control.

It also transmits the information for the live voting when you are interacting with your audience.

## Table of contents

- [Remote control](#remote-control)
  - [Receiver](#receiver)
  - [Emitter](#emitter)
- [Poll](#poll)
- [Develop](#develop)
- [License](#license)

## Remote control

This project is use to connect a presentation with the [DeckDeckGo's Progressive Web App](https://deckdeckgo.app) respectively the remote control.

The projects where the peers are implemented are the following:

### Receiver

The remote control "receiver" which should be included in the presentation:

| Source                                                                                     |
| ------------------------------------------------------------------------------------------ |
| [https://github.com/deckgo/deckdeckgo-remote](https://github.com/deckgo/deckdeckgo-remote) |

### Emitter

The remote control "emitter", the Progressive Web App, which lets you present and interact with your presentation:

| Application                                      |                                        Source                                        |
| ------------------------------------------------ | :----------------------------------------------------------------------------------: |
| [https://deckdeckgo.app](https://deckdeckgo.app) | [https://github.com/deckgo/deckdeckgo-app](https://github.com/deckgo/deckdeckgo-app) |

## Poll

This project also act as a middle point for the live voting when you are interacting with your audience.

The voting ("where your audience are recording their votes") is implemented in [DeckDeckGo].

The question and the display of the live results is implemented with a template in your presentation.

## Develop

If you would like to install and run locally this backend, proceed as following:

```
git clone https://github.com/deckgo/deckdeckgo
cd backend
npm install
npm run start
```

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[deckdeckgo]: https://deckdeckgo.com
