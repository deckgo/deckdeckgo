import * as io from 'socket.io-client';

import {BehaviorSubject, Observable, Subject} from 'rxjs';

// Types
import {
  DeckdeckgoEvent,
  DeckdeckgoEventSlideAction,
  DeckdeckgoEventDeck,
  DeckdeckgoEventSlide,
  DeckdeckgoEventSlideTo,
  DeckdeckgoEventNextPrevSlide
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
  maxPacketLifeTime: 1000, //milliseconds
};

const DEFAULT_SERVER_URL: string = 'https://api.deckdeckgo.com';

export enum ConnectionState {
  DISCONNECTED,
  CONNECTING,
  CONNECTED_WITH_SIGNALING_SERVER,
  CONNECTED,
  NOT_CONNECTED
}

// @ts-ignore
const PeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection || window.msRTCPeerConnection;

// @ts-ignore
const SessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription || window.msRTCSessionDescription;

export class CommunicationService {

  private static instance: CommunicationService;

  private socket: SocketIOClient.Socket;

  private rtcPeerConn: RTCPeerConnection;
  private dataChannelOut: RTCDataChannel;

  room: string;
  serverUrl: string;

  private state: BehaviorSubject<ConnectionState> = new BehaviorSubject<ConnectionState>(ConnectionState.DISCONNECTED);
  private event: Subject<DeckdeckgoEvent> = new Subject<DeckdeckgoEvent>();

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

      const url: string = this.serverUrl ? this.serverUrl : DEFAULT_SERVER_URL;

      this.state.next(ConnectionState.CONNECTING);

      this.socket = io.connect(url, {
        'transports': ['websocket', 'xhr-polling'],
        'query': 'type=app'
      });

      this.socket.on('connect', async () => {
        this.socket.emit('join', {
          room: this.room,
          deck: true
        });
      });

      this.socket.on('joined', async () => {
        // Do nothing on the deck side
        this.state.next(ConnectionState.CONNECTED_WITH_SIGNALING_SERVER);
      });

      this.socket.on('signaling_message', async (data) => {
        //Setup the RTC Peer Connection object
        if (!this.rtcPeerConn) {
          this.startSignaling();
        }

        if (data.type === 'app_here') {
          if (!this.rtcPeerConn.currentLocalDescription) {
            // let the 'negotiationneeded' event trigger offer generation
            await this.rtcPeerConn.createOffer().then((desc) => {
              this.sendLocalDesc(desc);
            }, (_err) => {
              this.state.next(ConnectionState.NOT_CONNECTED);
            });
          }

          return;
        }

        const message = JSON.parse(data.message);
        if (message.sdp) {
          this.rtcPeerConn.setRemoteDescription(new SessionDescription(message.sdp)).then(() => {
            // App create answer
          }, (_err) => {
            this.state.next(ConnectionState.NOT_CONNECTED);
          });
        } else {
          await this.rtcPeerConn.addIceCandidate(new RTCIceCandidate(message.candidate));
        }

      });

      this.socket.on('connect_error', () => {
        this.state.next(ConnectionState.NOT_CONNECTED);
      });

      this.socket.on('connect_timeout', () => {
        this.state.next(ConnectionState.NOT_CONNECTED);
      });

      this.socket.on('error', () => {
        this.state.next(ConnectionState.NOT_CONNECTED);
      });

      this.socket.on('reconnect_failed', () => {
        this.state.next(ConnectionState.NOT_CONNECTED);
      });

      this.socket.on('reconnect_error', () => {
        this.state.next(ConnectionState.NOT_CONNECTED);
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

      this.state.next(ConnectionState.DISCONNECTED);

      resolve();
    });
  }

  watchState(): Observable<ConnectionState> {
    return this.state.asObservable();
  }

  watchEvent(): Observable<DeckdeckgoEvent> {
    return this.event.asObservable();
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
          message: JSON.stringify({'candidate': evt.candidate}),
          room: this.room
        });
      }
    };
  }

  private sendLocalDesc(desc) {
    this.rtcPeerConn.setLocalDescription(desc).then(() => {
      this.socket.emit('signal', {
        type: 'sending_local_description',
        message: JSON.stringify({'sdp': this.rtcPeerConn.localDescription}),
        room: this.room
      });
    }, (_err) => {
      this.state.next(ConnectionState.NOT_CONNECTED);
    });
  };

  private dataChannelStateChanged = () => {
    if (this.dataChannelOut.readyState === 'open') {
      this.dataChannelOut.onmessage = this.receiveDataChannelMessage;
      this.state.next(ConnectionState.CONNECTED);
    }
  };

  private receiveDataChannelMessage = (event) => {
    if (!event) {
      return;
    }

    const data: DeckdeckgoEvent = JSON.parse(event.data);
    this.event.next(data);
  };

  emit(data: DeckdeckgoEvent | DeckdeckgoEventDeck | DeckdeckgoEventSlide | DeckdeckgoEventSlideTo | DeckdeckgoEventSlideAction | DeckdeckgoEventNextPrevSlide) {
    if (this.dataChannelOut) {
      this.dataChannelOut.send(JSON.stringify(data));
    }
  }

}
