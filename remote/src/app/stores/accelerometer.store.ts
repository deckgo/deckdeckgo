import {createStore} from '@stencil/store';

interface AccelerometerStore {
  enable: boolean;
  initialized: boolean;
  trigger: boolean | undefined;
}

const {state, onChange} = createStore({
  enable: false,
  initialized: false,
  trigger: undefined
} as AccelerometerStore);

export default {state, onChange};
