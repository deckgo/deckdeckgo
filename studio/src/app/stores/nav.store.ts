import {createStore} from '@stencil/store';

export enum NavDirection {
  FORWARD,
  RELOAD,
  BACK,
  ROOT
}

export interface NavParams {
  url?: string;
  direction: NavDirection;
}

interface NavStore {
  nav: NavParams | undefined;
}

const {state, onChange, reset} = createStore({
  nav: undefined
} as NavStore);

export default {state, onChange, reset};
