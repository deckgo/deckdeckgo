import {Component, Element, Listen, Prop, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {renderI18n} from '../../../utils/core/i18n.utils';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-deck-delete',
  styleUrl: 'app-deck-delete.scss'
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
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.core.sure}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <p>
          {renderI18n(i18n.state.dashboard.cannot_undone, {
            placeholder: '{0}',
            value: <strong>{this.deckName}</strong>
          })}
        </p>

        <form onSubmit={($event: Event) => this.handleSubmit($event)}>
          <ion-button type="submit" color="danger" class="ion-margin-top" shape="round">
            <ion-label>{i18n.state.dashboard.understand}</ion-label>
          </ion-button>
        </form>

        {this.renderNotePublished()}
      </ion-content>
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
