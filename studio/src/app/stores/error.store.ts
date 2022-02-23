import {createStore} from '@stencil/store';

interface ErrorStore {
  error: string | undefined;
}

const {state, onChange} = createStore({
  error: undefined
} as ErrorStore);

export default {state, onChange};
