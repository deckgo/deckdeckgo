import {createStore} from '@stencil/store';

import {TokenService} from '../services/data/token/token.service';

import {Token} from '../models/data/token';

interface TokenStore {
  token: Token | undefined;
}

const {state, onChange} = createStore({
  token: undefined,
} as TokenStore);

onChange('token', async (token: Token | undefined) => {
  await TokenService.getInstance().merge(token);
});

export default {state, onChange};
