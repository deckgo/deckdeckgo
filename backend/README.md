# DeckDeckGo - Backend

This project is the Backend of [DeckDeckGo]. Currently it acts as the signaling server to let [DeckDeckGo]'s peers establish a real-time communication channel using [WebRTC](https://webrtc.org).

## Table of contents

- [Peers](#peers)
	- [Receiver](#receiver)
	- [Emitter](#emitter)
- [Develop](#develop)
- [License](#license)

## Peers

Concretely this project is use to connect a presentation with the [DeckDeckGo's Progressive Web App](https://deckdeckgo.app) respectively the remote control.   

The projects where the peers are implemented are the following:

### Receiver

The remote control "receiver" which should be included in the presentation:

| Source |
| -------------------------- |
| [https://github.com/deckgo/deckdeckgo-remote](https://github.com/deckgo/deckdeckgo-remote) |

### Emitter

The remote control "emitter", the Progressive Web App, which lets you present and interact with your presentation:

| Application                      | Source |
| -------------------------- |:-----------------:|
|[https://deckdeckgo.app](https://deckdeckgo.app)|[https://github.com/deckgo/deckdeckgo-app](https://github.com/deckgo/deckdeckgo-app)|

## Develop

If you would like to install and run locally this backend, proceed as following:

```
git clone https://github.com/deckgo/deckdeckgo
cd backend
npm install
npm run start
```

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](nicolas@nmattia.com)

[DeckDeckGo]: https://deckdeckgo.com
