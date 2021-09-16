import {createStore} from '@stencil/store';

import {Template} from '@deckdeckgo/editor';

import templates from '../../assets/templates.json';

interface Templates {
  user: Template[];
  community: Template[];
}

const {state} = createStore<Templates>({
  user: [],
  community: templates.community
} as Templates);

export default {state};
