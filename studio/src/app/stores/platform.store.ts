import {createStore} from '@stencil/store';

import {PlatformService} from '../services/data/platform/platform.service';

import {Platform} from '../models/data/platform';
import {PlatformDeck} from '../models/data/platform-deck';

interface PlatformStore {
  platform: Platform | undefined;
  platformDeck: PlatformDeck | undefined;
}

const {state, onChange, reset} = createStore({
  platform: undefined,
  platformDeck: undefined,
} as PlatformStore);

onChange('platform', async (platform: Platform | undefined) => {
  await PlatformService.getInstance().merge(platform);
});

export default {state, onChange, reset};
