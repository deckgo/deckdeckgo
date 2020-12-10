import {Component, Event, EventEmitter, h, State} from '@stencil/core';

import {Template} from '../../../../models/data/template';

import {TemplateService} from '../../../../services/data/template/template.service';
import authStore from '../../../../stores/auth.store';
import errorStore from '../../../../stores/error.store';

@Component({
  tag: 'app-templates-user',
})
export class AppTemplatesUser {
  private templateService: TemplateService;

  @State()
  private templates: Template[] | undefined = undefined;

  @Event()
  navigateTemplates: EventEmitter<void>;

  @Event()
  selectedTemplate: EventEmitter<Template>;

  constructor() {
    this.templateService = TemplateService.getInstance();
  }

  async componentWillLoad() {
    await this.initTemplates();
  }

  private async initTemplates() {
    if (this.templates !== undefined) {
      return;
    }

    if (!authStore.state.authUser) {
      return;
    }

    try {
      const userTemplates: Template[] = await this.templateService.getUserTemplates(authStore.state.authUser.uid);
      this.templates = [...userTemplates];
    } catch (err) {
      errorStore.state.error = 'Templates can not be fetched.';
      this.templates = [];
    }
  }

  render() {
    if (this.templates !== undefined) {
      return this.renderTemplates();
    }

    return <div>Yolo</div>;
  }

  private renderTemplates() {
    return this.templates.map((template: Template) => {
      return (
        <app-template-showcase
          template={template}
          key={template.id}
          custom-tappable
          onClick={() => this.selectedTemplate.emit(template)}></app-template-showcase>
      );
    });
  }
}
