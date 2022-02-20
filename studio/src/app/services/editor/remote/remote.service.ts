import {Deck} from '@deckdeckgo/editor';
import {editorStore} from '@deckdeckgo/studio';
import {ConnectionState, DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';
import {Build} from '@stencil/core';
import {get, set} from 'idb-keyval';
import remoteStore from '../../../stores/remote.store';

export class RemoteService {
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

      if (remoteStore.state.remote !== remote) {
        remoteStore.state.remote = remote;
      }

      resolve();
    });
  }

  async switch(enable: boolean) {
    await set('deckdeckgo_remote', enable);
    remoteStore.state.remote = enable;
  }

  async getRoom(): Promise<string | null> {
    const deck: Deck | null = editorStore.default.state.deck;

    if (deck?.data?.name !== undefined && deck?.data?.name !== '') {
      return deck.data.name.replace(/\.|#/g, '_');
    }

    return null;
  }

  async addPendingRequests(request: DeckdeckgoEventDeckRequest) {
    let requests: DeckdeckgoEventDeckRequest[] = remoteStore.state.pendingRequests;
    if (!requests) {
      requests = [];
    }

    requests.push(request);

    remoteStore.state.pendingRequests = [...requests];
  }

  async shiftPendingRequests() {
    if (remoteStore.state.pendingRequests && remoteStore.state.pendingRequests.length > 0) {
      remoteStore.state.pendingRequests.shift();

      remoteStore.state.pendingRequests = [...remoteStore.state.pendingRequests];
    }
  }

  acceptRequest(request: DeckdeckgoEventDeckRequest) {
    remoteStore.state.acceptedRequest = {...request};
  }

  nextState(state: ConnectionState) {
    remoteStore.state.connectionState = state;
  }
}
