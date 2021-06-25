import {createStore} from '@stencil/store';

import assets from '../../assets/assets.json';

const {state} = createStore<Assets>({
  ...assets
} as Assets);

export default {state};
