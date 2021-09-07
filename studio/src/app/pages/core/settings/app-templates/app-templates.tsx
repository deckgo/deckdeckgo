import {Component, Fragment, h, State} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';

import authStore from '../../../../stores/auth.store';
import errorStore from '../../../../stores/error.store';
import templatesStore from '../../../../stores/templates.store';
import i18n from '../../../../stores/i18n.store';

import {Template} from '../../../../models/data/template';
import {AuthUser} from '../../../../models/auth/auth.user';

import {signIn} from '../../../../utils/core/signin.utils';

import {TemplateService} from '../../../../providers/data/template/template.service';
import {renderI18n} from '../../../../utils/core/i18n.utils';
import {modalController} from '../../../../utils/ionic/ionic.overlay';

@Component({
  tag: 'app-templates',
  styleUrl: 'app-templates.scss'
})
export class AppTemplates {
  private templateService: TemplateService;

  private destroyListener;

  @State()
  private loading: boolean = false;

  constructor() {
    this.templateService = TemplateService.getInstance();
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initTemplates();
    });

    await this.initTemplates();
  }

  private async initTemplates() {
    if (!authStore.state.authUser) {
      return;
    }

    this.destroyListener();

    try {
      this.loading = true;

      await this.templateService.init();
    } catch (err) {
      errorStore.state.error = 'Templates can not be fetched.';
    }

    this.loading = false;
  }

  private async editTemplate(template?: Template) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-template',
      componentProps: {
        template
      },
      cssClass: 'fullscreen'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        await this.createOrUpdateTemplate(detail.data);
      }
    });

    await modal.present();
  }

  private async createOrUpdateTemplate(template: Template) {
    try {
      if (template.id) {
        const updatedTemplate: Template = await this.templateService.update(template);
        templatesStore.state.user = [
          ...templatesStore.state.user.map((mapTemplate: Template) => (mapTemplate.id === updatedTemplate.id ? updatedTemplate : mapTemplate))
        ];
      } else {
        const createdTemplate: Template = await this.templateService.create(template.data);
        templatesStore.state.user = [createdTemplate, ...templatesStore.state.user];
      }
    } catch (err) {
      errorStore.state.error = 'Template can not be saved.';
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content class="ion-padding fullscreen-padding">
        <main class="ion-padding fit">
          <h1>{i18n.state.nav.templates}</h1>
          {this.renderGuardedContent()}
        </main>
      </ion-content>
    ];
  }

  private renderGuardedContent() {
    if (!authStore.state.authUser || authStore.state.anonymous) {
      return this.renderNotLoggedInContent();
    }

    if (this.loading) {
      return undefined;
    }

    return (
      <Fragment>
        {this.renderContent()}
        {this.renderAction()}
      </Fragment>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_templates, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  }

  private renderContent() {
    if (templatesStore.state.user.length === 0) {
      return <app-no-templates></app-no-templates>;
    }

    return (
      <Fragment>
        <ion-label>
          {renderI18n(i18n.state.settings.contribute_community, {
            placeholder: '{0}',
            value: (
              <a href="https://deckdeckgo.com/en/contact/" rel="noopener norefferer" target="_blank">
                {i18n.state.settings.contact}
              </a>
            )
          })}
        </ion-label>
        <div class="container ion-margin-top">{this.renderTemplates()}</div>
      </Fragment>
    );
  }

  private renderTemplates() {
    return templatesStore.state.user.map((template: Template) => {
      return <app-template-showcase template={template} editable={true} key={template.id} onClick={() => this.editTemplate(template)}></app-template-showcase>;
    });
  }

  private renderAction() {
    return (
      <div class="action ion-margin-top">
        <ion-button slot="end" shape="round" onClick={() => this.editTemplate()}>
          <ion-label>{i18n.state.settings.add_a_template}</ion-label>
        </ion-button>
      </div>
    );
  }
}
