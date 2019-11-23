import * as io from 'socket.io-client';

import {BehaviorSubject, Observable, Subject} from 'rxjs';
import {take} from 'rxjs/operators';

import {DeckdeckgoPollQuestion} from '@deckdeckgo/types';

export class CommunicationService {

  private socket: SocketIOClient.Socket;

  private pollKey: BehaviorSubject<string | undefined> = new BehaviorSubject<string | undefined>(undefined);
  private vote: Subject<string> = new Subject<string>();

  connect(url: string, path: string, poll: DeckdeckgoPollQuestion): Promise<void> {
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
        'reconnectionAttempts': 5,
        'transports': ['websocket', 'xhr-polling'],
        'query': 'type=app',
        'path': path
      });

      this.socket.on('connect', async () => {
        this.socket.emit('poll', {poll: poll});
      });

      this.socket.on('poll_key', async (data: string) => {
        this.pollKey.next(data);
      });

      this.socket.on('vote', async (answer: string) => {
        this.vote.next(answer);
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

      this.watchPollKey().pipe(take(1)).subscribe((key: string) => {
        if (key) {
          this.socket.emit('leave', {
            key: key
          });
        }

        this.socket.removeAllListeners();
        this.socket.disconnect();

        this.socket = undefined;

        resolve();
      });
    });
  }

  update(poll: DeckdeckgoPollQuestion): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.socket) {
        resolve();
        return;
      }

      this.watchPollKey().pipe(take(1)).subscribe((key: string) => {
        if (key) {
          this.socket.emit('update', {key: key, poll: poll});
        }
      });
    });
  }

  watchPollKey(): Observable<string | undefined> {
    return this.pollKey.asObservable();
  }

  watchVote(): Observable<string> {
    return this.vote.asObservable();
  }

}
