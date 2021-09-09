import {createStore} from '@stencil/store';

import {ConnectionState, DeckdeckgoEvent} from '@deckdeckgo/types';

interface RemoteStore {
  state: ConnectionState;
  $event: DeckdeckgoEvent | undefined;
}

const {state, onChange} = createStore({
  state: ConnectionState.DISCONNECTED,
  $event: undefined
} as RemoteStore);

export default {state, onChange};
