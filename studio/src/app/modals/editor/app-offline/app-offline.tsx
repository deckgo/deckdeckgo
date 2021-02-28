import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-offline',
  styleUrl: 'app-offline.scss',
})
export class AppNotes {
  @Element() el: HTMLElement;

  @Prop()
  offline: boolean = false;

  @State()
  private jobInProgress: boolean = false;

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    if (this.jobInProgress) {
      return;
    }

    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(false);
  }

  private onInProgress($event: CustomEvent) {
    if (!$event) {
      return;
    }

    this.jobInProgress = $event.detail;
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="tertiary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} disabled={this.jobInProgress} aria-label={i18n.state.core.close}>
              <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{this.offline ? 'Go Online' : 'Go Offline'}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding" color="light">
        <main class="ion-padding">{this.renderContent()}</main>
      </ion-content>,
    ];
  }

  private renderContent() {
    if (!this.offline) {
      return <app-go-offline onDoneOffline={() => this.closeModal()} onInProgress={($event: CustomEvent) => this.onInProgress($event)}></app-go-offline>;
    } else {
      return <app-go-online onDoneOnline={() => this.closeModal()} onInProgress={($event: CustomEvent) => this.onInProgress($event)}></app-go-online>;
    }
  }
}
