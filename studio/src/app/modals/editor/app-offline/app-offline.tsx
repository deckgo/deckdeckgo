import {Component, Element, h, Listen, Prop} from '@stencil/core';

@Component({
  tag: 'app-offline',
  styleUrl: 'app-offline.scss'
})
export class AppNotes {
  @Element() el: HTMLElement;

  @Prop()
  offline: boolean = false;

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(false);
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="tertiary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon name="close"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{this.offline ? 'Go Online' : 'Go Offline'}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding" color="light">
        <main class="ion-padding">{this.renderContent()}</main>
      </ion-content>
    ];
  }

  private renderContent() {
    if (!this.offline) {
      return <app-go-offline onDoneOffline={() => this.closeModal()}></app-go-offline>;
    } else {
      return <app-go-online onDoneOnline={() => this.closeModal()}></app-go-online>;
    }
  }
}
