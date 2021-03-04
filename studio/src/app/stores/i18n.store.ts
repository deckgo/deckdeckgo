import {createStore} from '@stencil/store';

import en from '../../assets/i18n/en.json';

const {state} = createStore<I18n>({
  lang: en,
  ...(en as Partial<I18n>),
} as I18n);

export default {state};
