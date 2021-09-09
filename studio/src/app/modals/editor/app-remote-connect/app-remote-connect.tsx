import {Component, Element, State, h, Listen, Fragment} from '@stencil/core';

import remoteStore from '../../../stores/remote.store';
import i18n from '../../../stores/i18n.store';

import {RemoteService} from '../../../services/editor/remote/remote.service';
import {renderI18n} from '../../../utils/core/i18n.utils';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-remote-connect',
  styleUrl: 'app-remote-connect.scss'
})
export class AppRemoteConnect {
  @Element() el: HTMLElement;

  private remoteService: RemoteService;

  @State()
  private qrCodeURI: string = 'https://deckdeckgo.app';

  constructor() {
    this.remoteService = RemoteService.getInstance();
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

  async componentWillLoad() {
    await this.initQRCodeURI();

    await this.initCloseOnConnected();
  }

  async disconnectedCallback() {
    await this.destroyCloseOnConnected();
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
      <Fragment>
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">
              <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
                <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.menu.remote_control}</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding">
          <p>{i18n.state.editor.remote}</p>
          <p class="no-padding-bottom">
            {renderI18n(i18n.state.editor.scan, {
              placeholder: '{0}',
              value: (
                <a href="https://deckdeckgo.app" target="_blank" rel="noopener noreferrer">
                  https://deckdeckgo.app <AppIcon name="open" ariaLabel="" ariaHidden={true}></AppIcon>
                </a>
              )
            })}
          </p>

          <div class="qrcode-container" style={remoteStore.state.remote ? {} : {opacity: '0.4'}}>
            <deckgo-qrcode content={this.qrCodeURI}>
              <AppIcon name="deckdeckgo-logo" path="img" ariaLabel="" ariaHidden={true}></AppIcon>
            </deckgo-qrcode>
          </div>
        </ion-content>
      </Fragment>
    );
  }
}
