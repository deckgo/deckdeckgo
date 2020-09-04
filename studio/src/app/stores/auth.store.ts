import {createStore} from '@stencil/store';

import {AuthUser} from '../models/auth/auth.user';

interface AuthStore {
  authUser: AuthUser | null;
  anonymous: boolean;
  bearer: string;
  loggedIn: boolean;
  gitHub: boolean;
}

const {state, onChange, reset} = createStore({
  authUser: null,
  anonymous: true,
  bearer: 'Bearer ',
  loggedIn: false,
  gitHub: false,
} as AuthStore);

onChange('authUser', (authUser: AuthUser) => {
  state.anonymous = authUser ? authUser.anonymous : true;

  // TODO: Remove bearer from store
  state.bearer = `Bearer ${authUser ? authUser.token : ''}`;

  state.loggedIn = authUser && !authUser.anonymous;
  state.gitHub = authUser ? authUser.gitHub : false;
});

export default {state, onChange, reset};
