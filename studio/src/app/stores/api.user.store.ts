import {createStore} from '@stencil/store';

import {ApiUser} from '../models/api/api.user';

interface ApitUserStore {
  apiUser: ApiUser | undefined;
}

const {state, onChange, reset} = createStore({
  apiUser: undefined,
} as ApitUserStore);

export default {state, onChange, reset};
