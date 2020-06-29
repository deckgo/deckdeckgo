import {createStore} from '@stencil/store';

export enum NavDirection {
  FORWARD,
  ROOT,
  BACK,
}

export interface NavParams {
  url?: string;
  direction: NavDirection;
}

interface NavStore {
  nav: NavParams | undefined;
}

const {state, onChange} = createStore({
  nav: undefined,
} as NavStore);

export default {state, onChange};
