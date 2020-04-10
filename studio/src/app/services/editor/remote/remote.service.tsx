import {BehaviorSubject, Observable} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {get, set} from 'idb-keyval';

import {DeckdeckgoEventDeckRequest, ConnectionState} from '@deckdeckgo/types';

import {Deck} from '../../../models/data/deck';

import {DeckEditorService} from '../deck/deck-editor.service';

export class RemoteService {
  private remoteSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);

  private remotePendingRequestsSubject: BehaviorSubject<DeckdeckgoEventDeckRequest[] | undefined> = new BehaviorSubject(undefined);

  private remoteStateSubject: BehaviorSubject<ConnectionState> = new BehaviorSubject(ConnectionState.DISCONNECTED);

  private static instance: RemoteService;

  private deckEditorService: DeckEditorService;

  private constructor() {
    // Private constructor, singleton
    this.deckEditorService = DeckEditorService.getInstance();
  }

  static getInstance() {
    if (!RemoteService.instance) {
      RemoteService.instance = new RemoteService();
    }
    return RemoteService.instance;
  }

  init(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const remote: boolean = await get<boolean>('deckdeckgo_remote');

      this.watch()
        .pipe(take(1))
        .subscribe((current: boolean) => {
          if (current !== remote) {
            this.remoteSubject.next(remote);
          }
        });

      resolve();
    });
  }

  async switch(enable: boolean) {
    await set('deckdeckgo_remote', enable);
    this.remoteSubject.next(enable);
  }

  watch(): Observable<boolean> {
    return this.remoteSubject.asObservable();
  }

  getRoom(): Promise<string> {
    return new Promise<string>((resolve) => {
      this.deckEditorService
        .watch()
        .pipe(
          filter((deck: Deck) => deck && deck.data && deck.data.name && deck.data.name !== undefined && deck.data.name !== ''),
          take(1)
        )
        .subscribe(async (deck: Deck) => {
          const roomName: string = deck.data.name.replace(/\.|#/g, '_');

          resolve(roomName);
        });
    });
  }

  async addPendingRequests(request: DeckdeckgoEventDeckRequest) {
    this.remotePendingRequestsSubject.pipe(take(1)).subscribe((requests: DeckdeckgoEventDeckRequest[] | undefined) => {
      if (!requests) {
        requests = [];
      }

      requests.push(request);

      this.remotePendingRequestsSubject.next(requests);
    });
  }

  watchRequests(): Observable<DeckdeckgoEventDeckRequest[] | undefined> {
    return this.remotePendingRequestsSubject.asObservable();
  }

  watchState(): Observable<ConnectionState> {
    return this.remoteStateSubject.asObservable();
  }

  nextState(state: ConnectionState) {
    this.remoteStateSubject.next(state);
  }
}
