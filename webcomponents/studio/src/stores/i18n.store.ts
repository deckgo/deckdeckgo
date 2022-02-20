import {createStore} from '@stencil/store';

interface I18nStore extends Record<string, Record<string, string>> {}

const {state, reset} = createStore<I18nStore>({});

export default {state, reset};
