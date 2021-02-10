import {createStore} from '@stencil/store';

import {BreadcrumbsStep} from '../types/editor/breadcrumbs-step';

interface EditorStore {
  step: BreadcrumbsStep;
  style: string | null;
}

const {state} = createStore<EditorStore>({
  step: BreadcrumbsStep.DECK,
  style: null,
});

export default {state};
