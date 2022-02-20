import {createStore} from '@stencil/store';
import {EnvironmentCloud} from '../types/env';

interface EnvStore {
  cloud: EnvironmentCloud | undefined;
}

const {state, reset} = createStore<EnvStore>({
  cloud: undefined
});

export default {state, reset};
