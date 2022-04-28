import {User, UserSocial} from '@deckdeckgo/editor';
import {userSubscribe} from '@deckdeckgo/sync';
import {createStore} from '@stencil/store';

interface UserStore {
  user: User | undefined;
  photoUrl: string | undefined;
  name: string | undefined;
  social: UserSocial | undefined;
}

const {state, onChange} = createStore({
  user: undefined,
  photoUrl: undefined,
  name: undefined,
  social: undefined
} as UserStore);

userSubscribe((user: User | undefined) => {
  state.user = user;
});

onChange('user', (user: User | undefined) => {
  state.photoUrl = user?.data?.photo_url ?? undefined;
  state.name = user?.data?.name ?? undefined;
  state.social = user?.data?.social ?? undefined;
});

const readonlyState: Readonly<typeof state> = state;

export default {state: readonlyState, onChange};
