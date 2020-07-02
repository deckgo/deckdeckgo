import {createStore} from '@stencil/store';

interface PublishStore {
  progress: number | undefined;
}

const {state} = createStore({
  progress: undefined,
} as PublishStore);

export default {state};
