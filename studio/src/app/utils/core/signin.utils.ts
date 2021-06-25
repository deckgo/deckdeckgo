import navStore, {NavDirection} from '../../stores/nav.store';

export const signIn = () => {
  navStore.state.nav = {
    url: '/signin' + (window.location?.pathname ?? ''),
    direction: NavDirection.FORWARD
  };
};
