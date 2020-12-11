import {Component, Fragment, h} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import authStore from '../../../../stores/auth.store';
import errorStore from '../../../../stores/error.store';
import navStore, {NavDirection} from '../../../../stores/nav.store';
import templatesStore from '../../../../stores/templates.store';

import {Template} from '../../../../models/data/template';
import {AuthUser} from '../../../../models/auth/auth.user';

import {TemplateService} from '../../../../services/data/template/template.service';

@Component({
  tag: 'app-templates',
  styleUrl: 'app-templates.scss',
})
export class AppTemplates {
  private templateService: TemplateService;

  private destroyListener;

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

  private async editTemplate(template?: Template) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-template',
      componentProps: {
        template,
      },
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
          ...templatesStore.state.user.map((mapTemplate: Template) => (mapTemplate.id === updatedTemplate.id ? updatedTemplate : mapTemplate)),
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
        <main class="ion-padding">
          <h1>Templates</h1>
          {this.renderGuardedContent()}
        </main>
      </ion-content>,
    ];
  }

  private renderGuardedContent() {
    if (!authStore.state.authUser || authStore.state.anonymous) {
      return this.renderNotLoggedInContent();
    }

    return (
      <Fragment>
        {this.renderContent()}
        {this.renderAction()}
      </Fragment>
    );
  }

  private renderNotLoggedInContent() {
    return [
      <p>
        <button type="button" class="app-button" onClick={() => this.signIn()}>
          Sign in
        </button>
        to upload and share templates.
      </p>,
    ];
  }

  private renderContent() {
    if (templatesStore.state.user.length === 0) {
      return <ion-label>You don't have any templates yet. Follow this guide to get started and add your first template afterwards.</ion-label>;
    }

    return <div class="container">{this.renderTemplates()}</div>;
  }

  private renderTemplates() {
    return templatesStore.state.user.map((template: Template) => {
      return <app-template-showcase template={template} key={template.id} onClick={() => this.editTemplate(template)}></app-template-showcase>;
    });
  }

  private renderAction() {
    return (
      <div class="action">
        <ion-button slot="end" shape="round" onClick={() => this.editTemplate()} class="ion-margin-top">
          <ion-label>Add a template</ion-label>
        </ion-button>
      </div>
    );
  }
}
