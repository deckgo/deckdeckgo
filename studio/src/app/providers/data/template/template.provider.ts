import {Template, TemplateData} from '@deckdeckgo/editor';

import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import authStore from '../../../stores/auth.store';
import templatesStore from '../../../stores/templates.store';

export const initTemplates = async () => {
  if (!authStore.state.authUser || authStore.state.authUser.anonymous) {
    return;
  }

  if (templatesStore.state.user?.length > 0) {
    return;
  }

  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  // TODO: Template for Internet Computer

  if (cloud !== 'firebase') {
    return;
  }

  try {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {getUserTemplates} = await import(cdn);

    const templates: Template[] = await getUserTemplates();

    if (!templates) {
      return undefined;
    }

    templatesStore.state.user = [...templates];
  } catch (err) {
    console.error(err);
  }
};

export const createUserTemplate = async (templateData: TemplateData): Promise<Template | undefined> => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  // TODO: Template for Internet Computer

  if (cloud !== 'firebase') {
    throw new Error('Template cannot be created. Not supported.');
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {createTemplate} = await import(cdn);

  return createTemplate(templateData);
};

export const updateTemplate = async (template: Template): Promise<Template | undefined> => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  // TODO: Template for Internet Computer

  if (cloud !== 'firebase') {
    throw new Error('Template cannot be updated. Not supported.');
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {updateTemplate} = await import(cdn);

  return updateTemplate(template);
};
