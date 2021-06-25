import {createStore} from '@stencil/store';

import {Deck} from '../models/data/deck';
import {UserSocial} from '../models/data/user';

export interface ShareData {
  deck: Deck | null;
  userName: string | undefined;
  userSocial: UserSocial | undefined;
}

interface ShareStore {
  share: ShareData;
}

const {state, onChange, reset} = createStore({
  share: null
} as ShareStore);

export default {state, onChange, reset};
