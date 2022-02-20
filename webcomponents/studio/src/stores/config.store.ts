import type {StyloConfig} from '@papyrs/stylo';
import {createStore} from '@stencil/store';
import {EnvironmentCloud} from '../types/env';

interface ConfigStore {
  cloud: EnvironmentCloud | undefined;
  stylo: Partial<StyloConfig>;
}

const {state, reset} = createStore<ConfigStore>({
  cloud: undefined,
  stylo: {}
});

export default {state, reset};
