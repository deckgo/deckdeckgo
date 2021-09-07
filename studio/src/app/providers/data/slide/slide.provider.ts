import {Slide} from '../../../models/data/slide';

import {SlideIcProvider} from './slide.ic.provider';
import {SlideFirebaseProvider} from './slide.firebase.provider';
import {SlideOfflineProvider} from './slide.offline.provider';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

export interface SlideProvider {
  get(deckId: string, slideId: string): Promise<Slide>;
}

export const getSlideService = (): SlideProvider => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');
  return cloud === 'ic' ? SlideIcProvider.getInstance() : cloud === 'firebase' ? SlideFirebaseProvider.getInstance() : SlideOfflineProvider.getInstance();
};
