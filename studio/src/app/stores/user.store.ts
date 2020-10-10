import {createStore} from '@stencil/store';

import {User, UserSocial} from '../models/data/user';

interface UserStore {
  user: User | undefined;
  photoUrl: string | undefined;
  loaded: boolean;
  name: string | undefined;
  social: UserSocial | undefined;
}

const {state, onChange, reset} = createStore({
  user: undefined,
  photoUrl: undefined,
  loaded: false,
  name: undefined,
  social: undefined,
} as UserStore);

onChange('user', (user: User | undefined) => {
  state.photoUrl = user.data?.photo_url ?? undefined;
  state.name = user.data?.name ?? undefined;
  state.social = user.data?.social ?? undefined;
  state.loaded = true;
});

export default {state, onChange, reset};
