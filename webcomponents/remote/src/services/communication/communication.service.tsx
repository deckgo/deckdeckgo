import * as io from 'socket.io-client';

import store from '../../stores/remote.store';

// Types
import {
  DeckdeckgoEvent,
  DeckdeckgoEventSlideAction,
  DeckdeckgoEventDeck,
  DeckdeckgoEventSlide,
  DeckdeckgoEventSlideTo,
  DeckdeckgoEventNextPrevSlide,
  DeckdeckgoEventDeckReveal,
  DeckdeckgoEventEmitter,
  DeckdeckgoEventDeckRequest,
  DeckdeckgoEventType,
  ConnectionState
} from '@deckdeckgo/types';

const configuration: RTCConfiguration = {
  iceServers: [
    {
      urls: 'turn:api.deckdeckgo.com:3478',
      username: 'user',
      credential: 'deckdeckgo'
    }
  ]
};

const dataChannelOptions = {
  ordered: false, //no guaranteed delivery, unreliable but faster
  maxPacketLifeTime: 1000 //milliseconds
};

const DEFAULT_SOCKET_URL: string = 'https://api.deckdeckgo.com';

// @ts-ignore
const PeerConnection =
  window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection || window.msRTCPeerConnection;

// @ts-ignore
// prettier-ignore
const SessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription || window.msRTCSessionDescription;

export class CommunicationService {
  private static instance: CommunicationService;

  private socket: SocketIOClient.Socket;

  private rtcPeerConn: RTCPeerConnection;
  private dataChannelOut: RTCDataChannel;

  room: string;
  socketUrl: string;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!CommunicationService.instance) {
      CommunicationService.instance = new CommunicationService();
    }
    return CommunicationService.instance;
  }

  connect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.room) {
        resolve();
        return;
      }

      const url: string = this.socketUrl ? this.socketUrl : DEFAULT_SOCKET_URL;

      store.state.state = ConnectionState.CONNECTING;

      this.socket = io.connect(url, {
        reconnectionAttempts: 5,
        transports: ['websocket', 'xhr-polling'],
        query: 'type=app'
      });

      this.socket.on('connect', async () => {
        this.socket.emit('join', {
          room: this.room,
          deck: true
        });
      });

      this.socket.on('joined', async () => {
        // Do nothing on the deck side
        store.state.state = ConnectionState.CONNECTED_WITH_SIGNALING_SERVER;
      });

      this.socket.on('signaling_message', async (data) => {
        //Setup the RTC Peer Connection object
        if (!this.rtcPeerConn) {
          this.startSignaling();
        }

        if (data.type === 'app_here') {
          store.state.$event = {
            type: DeckdeckgoEventType.DECK_REQUEST,
            emitter: DeckdeckgoEventEmitter.APP,
            message: data.message,
            fromSocketId: data.fromSocketId
          } as DeckdeckgoEventDeckRequest;

          return;
        }

        const message = JSON.parse(data.message);
        if (message.sdp) {
          this.rtcPeerConn.setRemoteDescription(new SessionDescription(message.sdp)).then(
            () => {
              // App create answer
            },
            (_err) => {
              store.state.state = ConnectionState.NOT_CONNECTED;
            }
          );
        } else {
          await this.rtcPeerConn.addIceCandidate(new RTCIceCandidate(message.candidate));
        }
      });

      this.socket.on('connect_error', () => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('connect_timeout', () => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('error', () => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('reconnect_failed', () => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('reconnect_error', () => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      });

      resolve();
    });
  }

  disconnect(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.dataChannelOut) {
        this.dataChannelOut.close();
      }

      if (this.rtcPeerConn) {
        this.rtcPeerConn.close();
      }

      this.dataChannelOut = null;
      this.rtcPeerConn = null;

      if (this.socket) {
        this.socket.emit('leave', {
          room: this.room
        });
        this.socket.removeAllListeners();
        this.socket.disconnect();
      }

      store.state.state = ConnectionState.DISCONNECTED;

      resolve();
    });
  }

  // Deck has selected an App to start the remote controlling
  async start(appSocketId: string) {
    if (!this.rtcPeerConn.currentLocalDescription) {
      // let the 'negotiationneeded' event trigger offer generation
      await this.rtcPeerConn.createOffer().then(
        (desc) => {
          this.sendLocalDesc(desc, appSocketId);
        },
        (_err) => {
          store.state.state = ConnectionState.NOT_CONNECTED;
        }
      );
    }

    return;
  }

  private startSignaling() {
    this.rtcPeerConn = new PeerConnection(configuration);

    this.dataChannelOut = this.rtcPeerConn.createDataChannel('deckgo_' + this.room, dataChannelOptions);
    this.dataChannelOut.onopen = this.dataChannelStateChanged;

    // send any ice candidates to the other peer
    this.rtcPeerConn.onicecandidate = (evt) => {
      if (evt.candidate) {
        this.socket.emit('signal', {
          type: 'ice_candidate',
          message: JSON.stringify({candidate: evt.candidate}),
          room: this.room
        });
      }
    };
  }

  private sendLocalDesc(desc, appSocketId: string) {
    this.rtcPeerConn.setLocalDescription(desc).then(
      () => {
        this.socket.emit('start', {
          type: 'sending_local_description',
          message: JSON.stringify({sdp: this.rtcPeerConn.localDescription}),
          room: this.room,
          toSocketId: appSocketId
        });
      },
      (_err) => {
        store.state.state = ConnectionState.NOT_CONNECTED;
      }
    );
  }

  private dataChannelStateChanged = () => {
    if (this.dataChannelOut.readyState === 'open') {
      this.dataChannelOut.onmessage = this.receiveDataChannelMessage;
      store.state.state = ConnectionState.CONNECTED;

      if (this.socket) {
        this.socket.emit('connected', {
          room: this.room
        });
      }
    }
  };

  private receiveDataChannelMessage = ($event) => {
    if (!$event) {
      return;
    }

    const data: DeckdeckgoEvent = JSON.parse($event.data);
    store.state.$event = {...data};
  };

  emit(
    data:
      | DeckdeckgoEvent
      | DeckdeckgoEventDeck
      | DeckdeckgoEventSlide
      | DeckdeckgoEventSlideTo
      | DeckdeckgoEventSlideAction
      | DeckdeckgoEventNextPrevSlide
      | DeckdeckgoEventDeckReveal
  ) {
    if (this.dataChannelOut) {
      this.dataChannelOut.send(JSON.stringify(data));
    }
  }
}
