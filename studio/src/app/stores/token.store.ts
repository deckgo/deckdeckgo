import {createStore} from '@stencil/store';

import {Token} from '@deckdeckgo/editor';

import {mergePlatformToken} from '../providers/data/platform/platform.provider';

interface TokenStore {
  token: Token | undefined;
}

const {state, onChange} = createStore({
  token: undefined
} as TokenStore);

onChange('token', async (token: Token | undefined) => {
  await mergePlatformToken(token);
});

export default {state, onChange};
