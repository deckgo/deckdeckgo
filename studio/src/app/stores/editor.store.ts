import {createStore} from '@stencil/store';

import {BreadcrumbsStep} from '../types/editor/breadcrumbs-step';

interface EditorStore {
  step: BreadcrumbsStep;
}

const {state} = createStore<EditorStore>({
  step: BreadcrumbsStep.DECK,
});

export default {state};
