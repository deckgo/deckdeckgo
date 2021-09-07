import {createStore} from '@stencil/store';

import {AuthUser} from '../models/auth/auth.user';

interface AuthStore {
  authUser: AuthUser | null;
  anonymous: boolean;
  loggedIn: boolean;
  gitHub: boolean;
}

const {state, onChange, reset} = createStore<AuthStore>({
  authUser: null,
  anonymous: true,
  loggedIn: false,
  gitHub: false
});

onChange('authUser', (authUser: AuthUser) => {
  state.anonymous = authUser ? authUser.anonymous : true;
  state.loggedIn = authUser && !authUser.anonymous;
  state.gitHub = authUser ? authUser.gitHub : false;
});

export default {state, onChange, reset};
