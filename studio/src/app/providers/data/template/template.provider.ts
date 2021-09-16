import {Template, TemplateData, GetUserTemplates, CreateTemplate, UpdateTemplate} from '@deckdeckgo/editor';

import authStore from '../../../stores/auth.store';
import templatesStore from '../../../stores/templates.store';

import {firebase} from '../../../utils/core/environment.utils';

export const initTemplates = async () => {
  if (!authStore.state.authUser || authStore.state.authUser.anonymous) {
    return;
  }

  if (templatesStore.state.user?.length > 0) {
    return;
  }

  // TODO: Template for Internet Computer

  if (!firebase()) {
    return;
  }

  try {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {getUserTemplates}: {getUserTemplates: GetUserTemplates} = await import(cdn);

    const templates: Template[] = await getUserTemplates(authStore.state.authUser?.uid);

    if (!templates) {
      return undefined;
    }

    templatesStore.state.user = [...templates];
  } catch (err) {
    console.error(err);
  }
};

export const createUserTemplate = async (templateData: TemplateData): Promise<Template | undefined> => {
  // TODO: Template for Internet Computer

  if (!firebase()) {
    throw new Error('Template cannot be created. Not supported.');
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {createTemplate}: {createTemplate: CreateTemplate} = await import(cdn);

  return createTemplate(templateData);
};

export const updateTemplate = async (template: Template): Promise<Template | undefined> => {
  // TODO: Template for Internet Computer

  if (!firebase()) {
    throw new Error('Template cannot be updated. Not supported.');
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {updateTemplate}: {updateTemplate: UpdateTemplate} = await import(cdn);

  return updateTemplate(template);
};
