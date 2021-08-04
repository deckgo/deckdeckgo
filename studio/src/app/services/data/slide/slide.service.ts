import {Slide} from '../../../models/data/slide';

import {SlideIcService} from './slide.ic.service';
import {SlideFirebaseService} from './slide.firebase.service';
import {SlideOfflineService} from './slide.offline.service';

import {EnvironmentConfigService} from '../../environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

export interface SlideService {
  get(deckId: string, slideId: string): Promise<Slide>;
}

export const getSlideService = (): SlideService => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');
  return cloud === 'ic' ? SlideIcService.getInstance() : cloud === 'firebase' ? SlideFirebaseService.getInstance() : SlideOfflineService.getInstance();
};
