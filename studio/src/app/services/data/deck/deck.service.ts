import {Deck} from '../../../models/data/deck';

import {DeckIcService} from './deck.ic.service';
import {DeckFirebaseService} from './deck.firebase.service';
import {DeckOfflineService} from './deck.offline.service';

import {EnvironmentConfigService} from '../../environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

export interface DeckService {
  entries(userId: string): Promise<Deck[]>;

  delete(deckId: string): Promise<void>;
}

export const getDeckService = (): DeckService => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');
  return cloud === 'ic' ? DeckIcService.getInstance() : cloud === 'firebase' ? DeckFirebaseService.getInstance() : DeckOfflineService.getInstance();
};
