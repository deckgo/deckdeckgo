import {Deck} from '@deckdeckgo/editor';

import {DeckIcProvider} from './deck.ic.provider';
import {DeckFirebaseProvider} from './deck.firebase.provider';
import {DeckOfflineProvider} from './deck.offline.provider';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

export interface DeckProvider {
  entries(userId: string): Promise<Deck[]>;

  delete(deckId: string): Promise<void>;
}

export const getDeckService = (): DeckProvider => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');
  return cloud === 'ic'
    ? DeckIcProvider.getInstance()
    : cloud === 'firebase'
    ? DeckFirebaseProvider.getInstance()
    : DeckOfflineProvider.getInstance();
};
