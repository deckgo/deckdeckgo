import {createStore} from '@stencil/store';

import en from '../../assets/i18n/en.json';
import {set} from 'idb-keyval';

const {state, onChange} = createStore<I18n>({
  lang: 'en',
  ...(en as Partial<I18n>),
} as I18n);

const esI18n = async (): Promise<I18n> => {
  return {
    lang: 'es',
    ...(await import(`../../assets/i18n/es.json`)),
  };
};

const enI18n = (): I18n => {
  return {
    lang: 'en',
    ...(en as Partial<I18n>),
  } as I18n;
};

onChange('lang', async (lang: 'en' | 'es') => {
  let bundle: I18n;

  switch (lang) {
    case 'es':
      bundle = await esI18n();
      break;
    default:
      bundle = enI18n();
  }

  Object.assign(state, bundle);
});

onChange('lang', (lang: 'en' | 'es') => {
  set('deckdeckgo_lang', lang).catch((err) => {
    console.error('Failed to update IDB with new language', err);
  });
});

export default {state};
