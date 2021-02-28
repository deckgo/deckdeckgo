import {createStore} from '@stencil/store';

import en from '../../assets/i18n/en.json';

interface I18n {
  lang: 'en';
  [index: string]: any;
}

const {state} = createStore<I18n>({
  lang: en,
  ...(en as any),
} as I18n);

export default {state};
