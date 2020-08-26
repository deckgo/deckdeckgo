import {createStore} from '@stencil/store';

import {PlatformService} from '../services/data/platform/platform.service';

import {Platform} from '../models/data/platform';

interface PlatformStore {
  platform: Platform | undefined;
}

const {state, onChange} = createStore({
  platform: undefined,
} as PlatformStore);

onChange('platform', async (platform: Platform | undefined) => {
  await PlatformService.getInstance().merge(platform);
});

export default {state, onChange};
