import {createStore} from '@stencil/store';

import {Deck} from '../models/data/deck';

export interface ShareData {
  deck: Deck | null;
  userName: string | undefined;
}

interface ShareStore {
  share: ShareData;
}

const {state, onChange, reset} = createStore({
  share: null,
} as ShareStore);

export default {state, onChange, reset};
