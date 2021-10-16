import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import en from '../../assets/i18n/en.json';

const {state, onChange} = createStore<I18n>({
  lang: 'en',
  ...(en as Partial<I18n>)
} as I18n);

const esI18n = async (): Promise<I18n> => {
  return {
    lang: 'es',
    ...(await import(`../../assets/i18n/es.json`))
  };
};

const deI18n = async (): Promise<I18n> => {
  return {
    lang: 'de',
    ...(await import(`../../assets/i18n/de.json`))
  };
};

const nlI18n = async (): Promise<I18n> => {
  return {
    lang: 'nl',
    ...(await import(`../../assets/i18n/nl.json`))
  };
};

const enI18n = (): I18n => {
  return {
    lang: 'en',
    ...(en as Partial<I18n>)
  } as I18n;
};

onChange('lang', async (lang: Languages) => {
  let bundle: I18n;

  switch (lang) {
    case 'es':
      bundle = await esI18n();
      break;
    case 'de':
      bundle = await deI18n();
      break;
    case 'nl':
      bundle = await nlI18n();
      break;
    default:
      bundle = enI18n();
  }

  Object.assign(state, bundle);
});

onChange('lang', (lang: Languages) => {
  set('deckdeckgo_lang', lang).catch((err) => {
    console.error('Failed to update IDB with new language', err);
  });
});

export default {state};
