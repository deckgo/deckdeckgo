import type {StyloConfig} from '@papyrs/stylo';
import {createStore} from '@stencil/store';

interface StyloStore {
  config: Partial<StyloConfig>;
}

const {state, reset} = createStore<StyloStore>({
  config: {}
});

export default {state, reset};
