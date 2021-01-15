import {Component, Event, EventEmitter, Fragment, h} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import templatesStore from '../../../../stores/templates.store';
import navStore, {NavDirection} from '../../../../stores/nav.store';

import {Template} from '../../../../models/data/template';
import {TemplateService} from '../../../../services/data/template/template.service';
import authStore from '../../../../stores/auth.store';

@Component({
  tag: 'app-templates-user',
  styleUrl: 'app-templates-user.scss',
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

  private async signIn() {
    navStore.state.nav = {
      url: '/signin' + (window.location?.pathname ?? ''),
      direction: NavDirection.FORWARD,
    };
  }

  render() {
    if (!authStore.state.loggedIn) {
      return this.renderNotLoggedIn();
    }

    return <Fragment>{this.renderTemplates()}</Fragment>;
  }

  private renderNotLoggedIn() {
    return (
      <Fragment>
        <p>DeckDeckGo provides a set of default and community templates for your slides but, you can also use your own.</p>

        <p>
          <button type="button" class="app-button" onClick={() => this.signIn()}>
            Sign in
          </button>
          to add yours.
        </p>
      </Fragment>
    );
  }

  private renderTemplates() {
    if (templatesStore.state.user.length === 0) {
      return (
        <p>
          <app-no-templates></app-no-templates>
        </p>
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
