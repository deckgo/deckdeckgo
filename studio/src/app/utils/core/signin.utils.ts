import navStore, {NavDirection} from '../../stores/nav.store';

import {AuthClient} from '@dfinity/auth-client';

export const signIn = async () => {
  // TODO: either firebase or ic

  navStore.state.nav = {
    url: '/signin' + (window.location?.pathname ?? ''),
    direction: NavDirection.FORWARD
  };

  const authClient = await AuthClient.create();

  await authClient.login({
    onSuccess: () => {
      console.log('**** AUTH 1 **** authClient now has an identity');
    },
    onError: (err?: string) => {
      console.log('**** AUTH 2 **** error while login in', err);
    }
  });
};
