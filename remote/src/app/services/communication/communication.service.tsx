import * as io from 'socket.io-client';

import remoteStore from '../../stores/remote.store';

// Types
import {
  DeckdeckgoEventDraw,
  DeckdeckgoEvent,
  DeckdeckgoEventNextPrevSlide,
  DeckdeckgoEventSlideAction,
  DeckdeckgoEventSlideTo,
  ConnectionState
} from '@deckdeckgo/types';

// Services
import {EnvironmentConfigService} from '../environment/environment-config.service';

const configuration: RTCConfiguration = {
  iceServers: [
    {
      urls: 'turn:api.deckdeckgo.com:3478',
      username: 'user',
      credential: 'deckdeckgo'
    }
  ]
};

// prettier-ignore
// @ts-ignore
const PeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection || window.msRTCPeerConnection;

// prettier-ignore
// @ts-ignore
const SessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription || window.msRTCSessionDescription;

export class CommunicationService {
  private static instance: CommunicationService;

  private socket: SocketIOClient.Socket;

  private rtcPeerConn: RTCPeerConnection;
  private dataChannelIn: RTCDataChannel;

  room: string;
  clientId: string = Math.floor(100000 + Math.random() * 900000)
    .toString()
    .replace(/\B(?=(\d{2})+(?!\d))/g, ' ');

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
      if (!window) {
        resolve();
        return;
      }

      remoteStore.state.state = ConnectionState.CONNECTING;

      const url: string = EnvironmentConfigService.getInstance().get('signalingServerUrl');

      this.socket = io.connect(url, {
        reconnectionAttempts: 5,
        transports: ['websocket', 'xhr-polling'],
        query: 'type=app'
      });

      this.socket.on('connect', async () => {
        this.socket.emit('rooms');
      });

      this.socket.on('active_rooms', async (data) => {
        remoteStore.state.rooms = [...data.rooms];
      });

      this.socket.on('joined', async () => {
        this.startSignaling();

        this.sendApp();

        remoteStore.state.state = ConnectionState.CONNECTED_WITH_SIGNALING_SERVER;
      });

      this.socket.on('deck_joined', async () => {
        if (!this.dataChannelIn || this.dataChannelIn.readyState === 'closed') {
          if (!this.rtcPeerConn) {
            this.startSignaling();
          }

          this.sendApp();
        }
      });

      this.socket.on('signaling_message', async (data) => {
        if (data.type !== 'app_here') {
          const message: any = JSON.parse(data.message);

          if (message.sdp) {
            this.rtcPeerConn.setRemoteDescription(new SessionDescription(message.sdp)).then(
              () => {
                // if we received an offer, we need to answer
                if (this.rtcPeerConn.remoteDescription.type === 'offer') {
                  this.rtcPeerConn.createAnswer().then((desc: RTCSessionDescriptionInit) => {
                    this.sendLocalDesc(desc);
                  });
                }
              },
              (_err) => {
                remoteStore.state.state = ConnectionState.NOT_CONNECTED;
              }
            );
          } else {
            await this.rtcPeerConn.addIceCandidate(new RTCIceCandidate(message.candidate));
          }
        }
      });

      this.socket.on('connect_error', () => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('connect_timeout', () => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('error', () => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('reconnect_failed', () => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      });

      this.socket.on('reconnect_error', () => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      });

      resolve();
    });
  }

  join() {
    if (!this.room) {
      return;
    }

    this.socket.emit('join', {
      room: this.room
    });
  }

  disconnect(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.dataChannelIn) {
        this.dataChannelIn.close();
      }

      if (this.rtcPeerConn) {
        this.rtcPeerConn.close();
      }

      this.dataChannelIn = null;
      this.rtcPeerConn = null;

      if (this.socket) {
        this.socket.emit('leave', {
          room: this.room
        });
        this.socket.removeAllListeners();
        this.socket.disconnect();
      }

      remoteStore.state.state = ConnectionState.DISCONNECTED;

      resolve();
    });
  }

  emit(data: DeckdeckgoEvent | DeckdeckgoEventDraw | DeckdeckgoEventNextPrevSlide | DeckdeckgoEventSlideAction | DeckdeckgoEventSlideTo) {
    if (this.dataChannelIn) {
      this.dataChannelIn.send(JSON.stringify(data));
    }
  }

  private startSignaling() {
    this.rtcPeerConn = new PeerConnection(configuration);

    this.rtcPeerConn.ondatachannel = this.receiveDataChannel;

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

    // Presentation create offer
  }

  private sendApp() {
    this.socket.emit('signal', {
      type: 'app_here',
      room: this.room,
      message: this.clientId
    });
  }

  private sendLocalDesc(desc) {
    this.rtcPeerConn.setLocalDescription(desc).then(
      () => {
        this.socket.emit('signal', {
          type: 'sending_local_description',
          message: JSON.stringify({sdp: this.rtcPeerConn.localDescription}),
          room: this.room
        });
      },
      (_err) => {
        remoteStore.state.state = ConnectionState.NOT_CONNECTED;
      }
    );
  }

  private receiveDataChannel = (event) => {
    this.dataChannelIn = event.channel;
    this.dataChannelIn.onmessage = this.receiveDataChannelMessage;
    this.dataChannelIn.onopen = this.connectedWhenOpened;
  };

  private connectedWhenOpened = () => {
    remoteStore.state.state = ConnectionState.CONNECTED;
  };

  private receiveDataChannelMessage = (event) => {
    if (!event) {
      return;
    }

    const data: DeckdeckgoEvent = JSON.parse(event.data);
    remoteStore.state.$event = {...data};
  };
}
