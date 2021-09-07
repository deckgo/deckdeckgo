import {createStore} from '@stencil/store';

import {PlatformService} from '../providers/data/platform/platform.service';

import {Token} from '../models/data/token';

interface TokenStore {
  token: Token | undefined;
}

const {state, onChange} = createStore({
  token: undefined
} as TokenStore);

onChange('token', async (token: Token | undefined) => {
  await PlatformService.getInstance().merge(token);
});

export default {state, onChange};
