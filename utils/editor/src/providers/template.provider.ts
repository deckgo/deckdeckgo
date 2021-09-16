import {Template, TemplateData} from '../models/data/template';

export interface GetUserTemplates {
  (userId: string): Promise<Template[]>;
}

export interface CreateTemplate {
  (templateData: TemplateData): Promise<Template>;
}

export interface UpdateTemplate {
  (template: Template): Promise<Template>;
}
