import {Component, Event, EventEmitter, Fragment, h} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import templatesStore from '../../../../stores/templates.store';

import {Template} from '../../../../models/data/template';
import {TemplateService} from '../../../../services/data/template/template.service';

@Component({
  tag: 'app-templates-user',
})
export class AppTemplatesUser {
  private templateService: TemplateService;

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
    try {
      await this.templateService.init();
    } catch (err) {
      errorStore.state.error = 'Templates can not be fetched.';
    }
  }

  render() {
    return (
      <Fragment>
        {this.renderTemplates()}

        <div>TODO: Add or link</div>
      </Fragment>
    );
  }

  private renderTemplates() {
    return templatesStore.state.user.map((template: Template) => {
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
