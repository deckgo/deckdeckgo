import {Build} from '@stencil/core';

import {BehaviorSubject, Observable, Subject} from 'rxjs';
import {take} from 'rxjs/operators';

import store from '../../../stores/deck.store';

import {get, set} from 'idb-keyval';

import {DeckdeckgoEventDeckRequest, ConnectionState} from '@deckdeckgo/types';

import {Deck} from '../../../models/data/deck';

export class RemoteService {
  private remoteSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);

  private remotePendingRequestsSubject: BehaviorSubject<DeckdeckgoEventDeckRequest[] | undefined> = new BehaviorSubject(undefined);

  private remoteStateSubject: BehaviorSubject<ConnectionState> = new BehaviorSubject(ConnectionState.DISCONNECTED);

  private remoteAcceptedRequestSubject: Subject<DeckdeckgoEventDeckRequest> = new Subject<DeckdeckgoEventDeckRequest>();

  private static instance: RemoteService;

  static getInstance() {
    if (!RemoteService.instance) {
      RemoteService.instance = new RemoteService();
    }
    return RemoteService.instance;
  }

  init(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (Build.isServer) {
        resolve();
        return;
      }

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

  async getRoom(): Promise<string | null> {
    const deck: Deck | null = store.state.deck;

    if (deck && deck.data && deck.data.name && deck.data.name !== undefined && deck.data.name !== '') {
      return deck.data.name.replace(/\.|#/g, '_');
    }

    return null;
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

  shiftPendingRequests(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.remotePendingRequestsSubject.pipe(take(1)).subscribe((requests: DeckdeckgoEventDeckRequest[] | undefined) => {
        if (requests && requests.length > 0) {
          requests.shift();

          this.remotePendingRequestsSubject.next(requests);
        }

        resolve();
      });
    });
  }

  acceptRequest(request: DeckdeckgoEventDeckRequest) {
    this.remoteAcceptedRequestSubject.next(request);
  }

  watchRequests(): Observable<DeckdeckgoEventDeckRequest[] | undefined> {
    return this.remotePendingRequestsSubject.asObservable();
  }

  watchState(): Observable<ConnectionState> {
    return this.remoteStateSubject.asObservable();
  }

  watchAcceptedRequest(): Observable<DeckdeckgoEventDeckRequest> {
    return this.remoteAcceptedRequestSubject.asObservable();
  }

  nextState(state: ConnectionState) {
    this.remoteStateSubject.next(state);
  }
}
