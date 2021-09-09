import {createStore} from '@stencil/store';

interface NotesStore {
  currentSlide: HTMLElement | undefined;
}

const {state, onChange} = createStore({
  currentSlide: undefined
} as NotesStore);

export default {state, onChange};
