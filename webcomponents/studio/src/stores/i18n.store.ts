import {createStore} from '@stencil/store';
import {en} from '../assets/i18n/en';
import {I18n, Languages} from '../types/i18n';

const {state, onChange} = createStore<I18n>(en);

const esI18n = async (): Promise<I18n> => {
  const {es} = await import(`../assets/i18n/es`);
  return es;
};

const deI18n = async (): Promise<I18n> => {
  const {de} = await import(`../assets/i18n/de`);
  return de;
};

const nlI18n = async (): Promise<I18n> => {
  const {nl} = await import(`../assets/i18n/nl`);
  return nl;
};

const enI18n = (): I18n => en;

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

export default {state};
