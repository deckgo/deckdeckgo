import {createStore} from '@stencil/store';

import {BreadcrumbsStep} from '../types/editor/breadcrumbs-step';

interface DeckEditorStore {
  step: BreadcrumbsStep;
  style: string | null;
}

const {state} = createStore<DeckEditorStore>({
  step: BreadcrumbsStep.DECK,
  style: null
});

export default {state};
