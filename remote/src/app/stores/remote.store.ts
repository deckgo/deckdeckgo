import {createStore} from '@stencil/store';

import {ConnectionState, DeckdeckgoEvent} from '@deckdeckgo/types';

export interface ActiveRoom {
  room: string;
  clients: number;
  connected: boolean;
}

interface RemoteStore {
  state: ConnectionState;
  $event: DeckdeckgoEvent | undefined;
  rooms: ActiveRoom[];
}

const {state, onChange} = createStore({
  state: ConnectionState.DISCONNECTED,
  $event: undefined,
  rooms: []
} as RemoteStore);

export default {state, onChange};
