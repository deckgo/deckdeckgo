import {createStore} from '@stencil/store';

import {PlatformProvider} from '../providers/data/platform/platform.provider';

import {Token} from '../models/data/token';

interface TokenStore {
  token: Token | undefined;
}

const {state, onChange} = createStore({
  token: undefined
} as TokenStore);

onChange('token', async (token: Token | undefined) => {
  await PlatformProvider.getInstance().merge(token);
});

export default {state, onChange};
