import {Component, Element, State, h} from '@stencil/core';

import remoteStore from '../../../../stores/remote.store';

import {RemoteService} from '../../../../services/editor/remote/remote.service';

@Component({
  tag: 'app-remote-connect',
  styleUrl: 'app-remote-connect.scss',
})
export class AppRemoteConnect {
  @Element() el: HTMLElement;

  private remoteService: RemoteService;

  @State()
  private qrCodeURI: string = 'https://deckdeckgo.app';

  constructor() {
    this.remoteService = RemoteService.getInstance();
  }

  async componentWillLoad() {
    await this.initQRCodeURI();

    await this.initCloseOnConnected();
  }

  async disconnectedCallback() {
    await this.destroyCloseOnConnected();
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  private initCloseOnConnected(): Promise<void> {
    return new Promise<void>((resolve) => {
      const deckgoRemoteElement = document.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      deckgoRemoteElement.addEventListener('state', this.onWatchState, {passive: true});

      resolve();
    });
  }

  private destroyCloseOnConnected(): Promise<void> {
    return new Promise<void>((resolve) => {
      const deckgoRemoteElement = document.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      deckgoRemoteElement.removeEventListener('state', this.onWatchState);

      resolve();
    });
  }

  private onWatchState = async ($event) => {
    // See ConnectionState.CONNECTED which is 3
    if ($event && $event.detail === 3) {
      await this.closePopover();
    }
  };

  async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async toggleRemoteEnabled() {
    await this.remoteService.switch(!remoteStore.state.remote);
  }

  private initQRCodeURI(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const room: string = await this.remoteService.getRoom();

      if (room && room !== undefined && room !== '') {
        this.qrCodeURI = `https://deckdeckgo.app/remote/${room}`;
      }

      resolve();
    });
  }

  render() {
    return (
      <div class="ion-padding">
        <p>Remote control your presentation with your phone or any devices.</p>
        <p class="no-padding-bottom">
          Scan the QR-Code or get the Progressive Web Apps at{' '}
          <a href="https://deckdeckgo.app" target="_blank" rel="noopener noreferrer">
            https://deckdeckgo.app <ion-icon name="open"></ion-icon>
          </a>
        </p>

        <div class="qrcode-container">
          <deckgo-qrcode content={this.qrCodeURI}>
            <ion-icon slot="logo" src="/assets/img/deckdeckgo-logo.svg"></ion-icon>
          </deckgo-qrcode>
        </div>

        <ion-list>
          <ion-item>
            {this.renderLabel()}
            <ion-toggle slot="end" mode="md" checked={remoteStore.state.remote} onIonChange={() => this.toggleRemoteEnabled()}></ion-toggle>
          </ion-item>
        </ion-list>

        <p>
          <small>If you can't connect or loose the connection, toggle off and on the remote to restart.</small>
        </p>
      </div>
    );
  }

  private renderLabel() {
    if (remoteStore.state.remote) {
      return (
        <ion-label>
          Remote is <strong>enabled</strong>
        </ion-label>
      );
    } else {
      return (
        <ion-label>
          Remote is <strong>disabled</strong>
        </ion-label>
      );
    }
  }
}
