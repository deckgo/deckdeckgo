import {Component, Element, Fragment, h, Listen, Prop} from '@stencil/core';

import authStore from '../../../stores/auth.store';

import {Template, TemplateData} from '../../../models/data/template';

@Component({
  tag: 'app-template',
  styleUrl: 'app-template.scss',
})
export class AppTemplate {
  @Element() el: HTMLElement;

  @Prop()
  template: Template | undefined;

  private templateData: TemplateData | undefined;

  async componentWillLoad() {
    if (!this.template || !this.template.data) {
      this.templateData = {
        owner_id: authStore.state.authUser.uid,
      };
      return;
    }

    this.templateData = this.template.data;
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async handleSubmit($event: Event) {
    $event.preventDefault();

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss({
      ...this.template,
      data: {
        ...this.templateData,
      },
    });
  }

  private async onCdnInput($event: CustomEvent<KeyboardEvent>) {
    if (!this.templateData) {
      return;
    }

    this.templateData.cdn = ($event.target as InputTargetEvent).value;
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">
              <ion-button onClick={() => this.closeModal()}>
                <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">Template</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding">
          <form onSubmit={(e: Event) => this.handleSubmit(e)}>
            <ion-list class="inputs-list">
              <ion-item class="item-title">
                <ion-label>CDN</ion-label>
              </ion-item>

              <ion-item>
                <ion-input
                  value={this.templateData?.cdn}
                  debounce={500}
                  minlength={3}
                  required={true}
                  input-mode="text"
                  onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCdnInput($event)}></ion-input>
              </ion-item>
            </ion-list>

            <ion-button type="submit" color="primary" class="ion-margin-top" shape="round">
              <ion-label>Save</ion-label>
            </ion-button>
          </form>
        </ion-content>
      </Fragment>
    );
  }
}
