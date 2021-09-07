import {Component, Event, EventEmitter, Fragment, h} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import templatesStore from '../../../../stores/templates.store';
import i18n from '../../../../stores/i18n.store';

import {signIn} from '../../../../utils/core/signin.utils';

import {Template} from '../../../../models/data/template';
import {TemplateProvider} from '../../../../providers/data/template/template.provider';
import authStore from '../../../../stores/auth.store';
import {renderI18n} from '../../../../utils/core/i18n.utils';

@Component({
  tag: 'app-templates-user',
  styleUrl: 'app-templates-user.scss'
})
export class AppTemplatesUser {
  private templateProvider: TemplateProvider;

  @Event()
  navigateSignIn: EventEmitter<void>;

  @Event()
  selectedTemplate: EventEmitter<Template>;

  constructor() {
    this.templateProvider = TemplateProvider.getInstance();
  }

  async componentWillLoad() {
    await this.initTemplates();
  }

  private async initTemplates() {
    try {
      await this.templateProvider.init();
    } catch (err) {
      errorStore.state.error = 'Templates can not be fetched.';
    }
  }

  private signIn() {
    signIn();

    this.navigateSignIn.emit();
  }

  render() {
    if (!authStore.state.loggedIn) {
      return this.renderNotLoggedIn();
    }

    return <Fragment>{this.renderTemplates()}</Fragment>;
  }

  private renderNotLoggedIn() {
    return (
      <div class="info ion-padding-start ion-padding-end">
        <p>{i18n.state.templates.set_of_default}</p>

        <p>
          {renderI18n(i18n.state.templates.sign_in_to_add, {
            placeholder: '{0}',
            value: (
              <button type="button" class="app-button" onClick={() => this.signIn()}>
                {i18n.state.nav.sign_in}
              </button>
            )
          })}
        </p>
      </div>
    );
  }

  private renderTemplates() {
    if (templatesStore.state.user.length === 0) {
      return (
        <div class="info ion-padding">
          <app-no-templates></app-no-templates>
        </div>
      );
    }

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
