import {User, UserSocial} from '@deckdeckgo/editor';
import {createStore} from '@stencil/store';

interface UserStore {
  user: User | undefined;
  photoUrl: string | undefined;
  name: string | undefined;
  social: UserSocial | undefined;
}

const {state, onChange, reset} = createStore({
  user: undefined,
  photoUrl: undefined,
  name: undefined,
  social: undefined
} as UserStore);

onChange('user', (user: User | undefined) => {
  state.photoUrl = user?.data?.photo_url ?? undefined;
  state.name = user?.data?.name ?? undefined;
  state.social = user?.data?.social ?? undefined;
});

export default {state, onChange, reset};
