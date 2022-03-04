import {DeckOrDoc, deleteDeckOrDoc as deleteDeckOrDocService, removeSyncBeforeUnload} from '@deckdeckgo/sync';
import navStore, {NavDirection} from '../../stores/nav.store';
import {firebase} from './environment.utils';

export const navigateReloadEditor = () => {
  // We are aware a sync is going to happen and we are navigating programmatically
  removeSyncBeforeUnload();

  navStore.state.nav = {
    url: '/',
    direction: NavDirection.RELOAD
  };
};

export const deleteDeckOrDoc = async (data: DeckOrDoc) => await deleteDeckOrDocService({data, deleteStorage: !firebase()});
