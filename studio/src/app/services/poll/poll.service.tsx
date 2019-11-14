import * as io from 'socket.io-client';

import {BehaviorSubject, Observable} from 'rxjs';
import {take} from 'rxjs/operators';

import {Poll} from '../../models/poll/poll';

export class PollService {

  private socket: SocketIOClient.Socket;

  private poll: BehaviorSubject<Poll | undefined> = new BehaviorSubject<Poll|undefined>(undefined);

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
      if (!pollKey || pollKey === undefined || pollKey  === '') {
        resolve();
        return;
      }

      const url: string = 'http://localhost:3003';

      this.socket = io.connect(url, {
        'transports': ['websocket', 'xhr-polling'],
        'query': 'type=app'
      });

      this.socket.on('connect', async () => {
        this.socket.emit('join', {key: pollKey});
      });

      this.socket.on('poll_desc', async (data) => {
        this.poll.next(data);
      });

      resolve();
    });
  }

  vote(key: string, answer: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.socket) {
        resolve();
        return;
      }

      this.socket.emit('vote', {
        key: key,
        answer: answer
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

      this.watch().pipe(take(1)).subscribe((poll: Poll) => {
        if (poll) {
          this.socket.emit('leave', {
            key: poll.key
          });
        }

        this.socket.removeAllListeners();
        this.socket.disconnect();

        resolve();
      });
    });
  }

  watch(): Observable<Poll | undefined> {
    return this.poll.asObservable();
  }

}
