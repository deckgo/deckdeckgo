import {createStore} from '@stencil/store';

import {Template} from '../models/data/template';

interface Templates {
  community: Template[];
}

const {state} = createStore<Templates>({
  community: [],
} as Templates);

export default {state};
