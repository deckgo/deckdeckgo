import navStore, {NavDirection} from '../../stores/nav.store';

export const signIn = () => {
  navStore.state.nav = {
    url: '/signin',
    direction: NavDirection.FORWARD
  };
};
