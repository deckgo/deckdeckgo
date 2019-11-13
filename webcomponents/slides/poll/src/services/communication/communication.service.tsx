import * as io from 'socket.io-client';

import {BehaviorSubject, Observable} from 'rxjs';
import {take} from 'rxjs/operators';

export class CommunicationService {

  private socket: SocketIOClient.Socket;

  private pollKey: BehaviorSubject<string | undefined> = new BehaviorSubject<string | undefined>(undefined);

  private static instance: CommunicationService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!CommunicationService.instance) {
      CommunicationService.instance = new CommunicationService();
    }

    return CommunicationService.instance;
  }

  connect(poll): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const url: string = 'http://localhost:3003';

      this.socket = io.connect(url, {
        'transports': ['websocket', 'xhr-polling'],
        'query': 'type=app'
      });

      this.socket.on('connect', async () => {
        this.socket.emit('poll', {poll: poll});
      });

      this.socket.on('poll_key', async (data) => {
        this.pollKey.next(data);
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
      });

      resolve();
    });
  }

  watchPollKey(): Observable<string | undefined> {
    return this.pollKey.asObservable();
  }

}
