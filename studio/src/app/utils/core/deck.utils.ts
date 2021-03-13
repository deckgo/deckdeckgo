import authStore from '../../stores/auth.store';

import {Utils} from './utils';

import {DeckData} from '../../models/data/deck';

export const initDeckData = async (): Promise<DeckData> => {
  return {
    name: `Presentation ${await Utils.getNow()}`,
    owner_id: authStore.state.authUser.uid,
  };
};
