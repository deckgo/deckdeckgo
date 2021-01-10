import * as io from 'socket.io-client';

import store from '../../stores/poll.store';

import {DeckdeckgoPoll} from '@deckdeckgo/types';

import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../core/environment/environment-config.service';

export class PollService {
  private socket: SocketIOClient.Socket;

  private static instance: PollService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!PollService.instance) {
      PollService.instance = new PollService();
    }

    return PollService.instance;
  }

  connect(pollKey: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!pollKey || pollKey === undefined || pollKey === '') {
        resolve();
        return;
      }

      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      this.socket = io.connect(config.socketUrl, {
        reconnectionAttempts: 5,
        transports: ['websocket', 'xhr-polling'],
        query: 'type=app',
        path: '/poll',
      });

      this.socket.on('connect', async () => {
        this.socket.emit('join', {key: pollKey});
      });

      this.socket.on('poll_desc', async (data: DeckdeckgoPoll) => {
        store.state.poll = {...data};
      });

      resolve();
    });
  }

  vote(key: string, answer: string): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      if (!this.socket || !this.socket.connected || this.socket.disconnected) {
        reject('Vote could not be registered, poll server is not reachable.');
        return;
      }

      this.socket.emit('vote', {
        key: key,
        answer: answer,
      });

      resolve();
    });
  }

  disconnect(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.socket) {
        resolve();
        return;
      }

      if (store.state.poll) {
        this.socket.emit('leave', {
          key: store.state.poll.key,
        });
      }

      this.socket.removeAllListeners();
      this.socket.disconnect();

      resolve();
    });
  }
}
