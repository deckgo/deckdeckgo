import {createStore} from '@stencil/store';

import {Deploy} from '../models/data/deploy';

interface DeployStore {
  deploy: Deploy | undefined;
}

const {state, onChange, reset} = createStore({
  deploy: undefined,
} as DeployStore);

export default {state, onChange, reset};
