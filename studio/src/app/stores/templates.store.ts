import {createStore} from '@stencil/store';

import {Template} from '../models/data/template';

interface Templates {
  user: Template[];
}

const {state} = createStore<Templates>({
  user: [],
} as Templates);

export default {state};
