import * as io from 'socket.io-client';

import {DeckdeckgoPollQuestion, DeckdeckgoPoll} from '@deckdeckgo/types';

export class CommunicationService {
  private socket: SocketIOClient.Socket;

  connect(url: string, path: string, poll: DeckdeckgoPollQuestion, updatePollKey: Function, updateVote: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.socket) {
        resolve();
        return;
      }

      if (!url || url === undefined) {
        resolve();
        return;
      }

      this.socket = io.connect(url, {
        reconnectionAttempts: 5,
        transports: ['websocket', 'xhr-polling'],
        query: 'type=app',
        path: path,
      });

      this.socket.on('connect', async () => {
        this.socket.emit('poll', {poll: poll});
      });

      this.socket.on('poll_key', async (data: string) => {
        updatePollKey(data);
      });

      this.socket.on('vote', async (answer: string) => {
        updateVote(answer);
      });

      resolve();
    });
  }

  retrieve(url: string, path: string, pollKey: string, updatePoll: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.socket) {
        resolve();
        return;
      }

      if (!url || url === undefined) {
        resolve();
        return;
      }

      this.socket = io.connect(url, {
        reconnectionAttempts: 5,
        transports: ['websocket', 'xhr-polling'],
        query: 'type=app',
        path: path,
      });

      this.socket.on('connect', async () => {
        this.socket.emit('join', {key: pollKey});
      });

      this.socket.on('poll_desc', async (data: DeckdeckgoPoll) => {
        updatePoll(data);
      });

      this.socket.on('poll_updated', async (data: DeckdeckgoPoll) => {
        updatePoll({
          key: pollKey,
          poll: data,
        });
      });

      resolve();
    });
  }

  disconnect(pollKey: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.socket) {
        resolve();
        return;
      }

      if (pollKey) {
        this.socket.emit('leave', {
          key: pollKey,
        });
      }

      this.socket.removeAllListeners();
      this.socket.disconnect();

      this.socket = undefined;

      resolve();
    });
  }

  update(poll: DeckdeckgoPollQuestion, pollKey: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.socket) {
        resolve();
        return;
      }

      if (pollKey) {
        this.socket.emit('update', {key: pollKey, poll: poll});
      }
    });
  }
}
