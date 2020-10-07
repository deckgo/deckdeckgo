import {createStore} from '@stencil/store';

import {ApiUser} from '../models/api/api.user';

interface ApiUserStore {
  apiUser: ApiUser | undefined;
}

const {state, onChange, reset} = createStore({
  apiUser: undefined,
} as ApiUserStore);

export default {state, onChange, reset};
