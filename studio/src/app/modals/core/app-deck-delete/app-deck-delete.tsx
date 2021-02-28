import {Component, Element, Listen, Prop, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-deck-delete',
  styleUrl: 'app-deck-delete.scss',
})
export class AppDeckDelete {
  @Element() el: HTMLElement;

  @Prop()
  deckName: string;

  @Prop()
  published: string;

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

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(true);
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="danger">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Are you absolutely sure?</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <p>
          This action cannot be undone. This will permanently delete your presentation <strong>{this.deckName}</strong>.
        </p>

        <form onSubmit={($event: Event) => this.handleSubmit($event)}>
          <ion-button type="submit" color="danger" class="ion-margin-top" shape="round">
            <ion-label>I understand, delete my presentation</ion-label>
          </ion-button>
        </form>

        {this.renderNotePublished()}
      </ion-content>,
    ];
  }

  private renderNotePublished() {
    if (this.published) {
      return <app-unpublish></app-unpublish>;
    } else {
      return undefined;
    }
  }
}
