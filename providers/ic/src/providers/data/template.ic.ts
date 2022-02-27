import {CreateTemplate, GetUserTemplates, Template, TemplateData, UpdateTemplate} from '@deckdeckgo/editor';
import {nanoid} from 'nanoid';
import {entries, setData} from '../../utils/data.utils';

export const getUserTemplates: GetUserTemplates = (_userId: string): Promise<Template[]> =>
  entries<Template, TemplateData>({startsWith: '/templates/'});

export const createTemplate: CreateTemplate = (data: TemplateData): Promise<Template> => {
  const id: string = nanoid();

  return setData<Template, TemplateData>({key: `/templates/${id}`, id, data});
};

export const updateTemplate: UpdateTemplate = (template: Template): Promise<Template> => {
  const {data, id} = template;

  return setData<Template, TemplateData>({key: `/templates/${id}`, id, data});
};
