import {createStore} from '@stencil/store';

import {Template} from '../models/data/template';

import templates from '../../assets/templates.json';

interface Templates {
  user: Template[];
  community: Template[];
}

const {state} = createStore<Templates>({
  user: [],
  community: templates.community,
} as Templates);

export default {state};
