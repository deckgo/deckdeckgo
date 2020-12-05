import {Component, Fragment, h, State} from '@stencil/core';

import authStore from '../../../../stores/auth.store';
import errorStore from '../../../../stores/error.store';
import navStore, {NavDirection} from '../../../../stores/nav.store';

import {Template} from '../../../../models/data/template';

import {TemplateService} from '../../../../services/data/template/template.service';
import {AuthUser} from '../../../../models/auth/auth.user';
import {modalController, OverlayEventDetail} from '@ionic/core';

@Component({
  tag: 'app-templates',
  styleUrl: 'app-templates.scss',
})
export class AppTemplates {
  private templateService: TemplateService;

  @State()
  private templates: Template[] | undefined;

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
      const userTemplates: Template[] = await this.templateService.getUserTemplates(authStore.state.authUser.uid);
      this.templates = [...userTemplates];
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

  private async editTemplate() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-template',
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
        this.templates = [...this.templates.map((mapTemplate: Template) => (mapTemplate.id === updatedTemplate.id ? updatedTemplate : mapTemplate))];
      } else {
        const createdTemplate: Template = await this.templateService.create(template.data);
        this.templates = [createdTemplate, ...this.templates];
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
    if (!this.templates) {
      return undefined;
    }

    if (this.templates.length === 0) {
      return <ion-label>You don't have any templates yet. Follow this guide to get started and add your first template afterwards.</ion-label>;
    }

    return <div>TODO</div>;
  }

  private renderAction() {
    if (!this.templates) {
      return undefined;
    }

    return (
      <div class="action">
        <ion-button slot="end" shape="round" onClick={() => this.editTemplate()} class="ion-margin-top">
          <ion-label>Add a template</ion-label>
        </ion-button>
      </div>
    );
  }
}
