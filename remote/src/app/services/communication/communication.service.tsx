import * as io from 'socket.io-client';

import {BehaviorSubject, Observable, Subject} from 'rxjs';

// Types
import {
    DeckdeckgoEventDraw,
    DeckdeckgoEvent,
    DeckdeckgoEventNextPrevSlide,
    DeckdeckgoEventSlideAction,
    DeckdeckgoEventSlideTo
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

export enum ConnectionState {
    DISCONNECTED,
    CONNECTING,
    CONNECTED_WITH_SIGNALING_SERVER,
    CONNECTED,
    NOT_CONNECTED
}

export interface ActiveRoom {
    room: string;
    clients: number;
}

// @ts-ignore
const PeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection || window.msRTCPeerConnection;

// @ts-ignore
const SessionDescription = window.RTCSessionDescription || window.mozRTCSessionDescription || window.webkitRTCSessionDescription || window.msRTCSessionDescription;

export class CommunicationService {

    private static instance: CommunicationService;

    private socket: SocketIOClient.Socket;

    private rtcPeerConn: RTCPeerConnection;
    private dataChannelIn: RTCDataChannel;

    room: string;

    private state: BehaviorSubject<ConnectionState> = new BehaviorSubject<ConnectionState>(ConnectionState.DISCONNECTED);
    private event: Subject<DeckdeckgoEvent> = new Subject<DeckdeckgoEvent>();
    private rooms: BehaviorSubject<ActiveRoom[]> = new BehaviorSubject<ActiveRoom[]>([]);

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

            this.state.next(ConnectionState.CONNECTING);

            const url: string = EnvironmentConfigService.getInstance().get('signalingServerUrl');

            this.socket = io.connect(url, {
                'transports': ['websocket', 'xhr-polling'],
                'query': 'type=app'
            });

            this.socket.on('connect', async () => {
                this.socket.emit('rooms');
            });

            this.socket.on('active_rooms', async (data) => {
                this.rooms.next(data.rooms);
            });

            this.socket.on('joined', async () => {
                this.startSignaling();

                this.sendApp();

                this.state.next(ConnectionState.CONNECTED_WITH_SIGNALING_SERVER);
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
                        this.rtcPeerConn.setRemoteDescription(new SessionDescription(message.sdp)).then(() => {
                            // if we received an offer, we need to answer
                            if (this.rtcPeerConn.remoteDescription.type === 'offer') {
                                this.rtcPeerConn.createAnswer().then((desc: RTCSessionDescriptionInit) => {
                                    this.sendLocalDesc(desc);
                                })
                            }
                        }, (_err) => {
                            this.state.next(ConnectionState.NOT_CONNECTED);
                        });
                    } else {
                        await this.rtcPeerConn.addIceCandidate(new RTCIceCandidate(message.candidate));
                    }
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

            this.state.next(ConnectionState.DISCONNECTED);

            resolve();
        });
    }

    emit(data: DeckdeckgoEvent | DeckdeckgoEventDraw | DeckdeckgoEventNextPrevSlide | DeckdeckgoEventSlideAction | DeckdeckgoEventSlideTo) {
        if (this.dataChannelIn) {
            this.dataChannelIn.send(JSON.stringify(data));
        }
    }

    watchState(): Observable<ConnectionState> {
        return this.state.asObservable();
    }

    watchEvent(): Observable<DeckdeckgoEvent> {
        return this.event.asObservable();
    }

    watchRooms(): Observable<ActiveRoom[]> {
        return this.rooms.asObservable();
    }

    private startSignaling() {
        this.rtcPeerConn = new PeerConnection(configuration);

        this.rtcPeerConn.ondatachannel = this.receiveDataChannel;

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

        // Presentation create offer
    }

    private sendApp() {
        this.socket.emit('signal', {
            type: 'app_here',
            room: this.room
        });
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

    private receiveDataChannel = (event) => {
        this.dataChannelIn = event.channel;
        this.dataChannelIn.onmessage = this.receiveDataChannelMessage;
        this.dataChannelIn.onopen = this.connectedWhenOpened;
    };

    private connectedWhenOpened = () => {
        this.state.next(ConnectionState.CONNECTED);
    };

    private receiveDataChannelMessage = (event) => {
        if (!event) {
            return;
        }

        const data: DeckdeckgoEvent = JSON.parse(event.data);
        this.event.next(data);
    };

}
