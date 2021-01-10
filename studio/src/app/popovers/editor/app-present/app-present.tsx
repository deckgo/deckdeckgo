import {Component, Element, EventEmitter, h, Prop, State, Event, Fragment} from '@stencil/core';

import {isIPad, isMobile} from '@deckdeckgo/utils';

import remoteStore from '../../../stores/remote.store';

import {MoreAction} from '../../../types/editor/more-action';

import {RemoteService} from '../../../services/editor/remote/remote.service';

@Component({
  tag: 'app-present',
  styleUrl: 'app-present.scss',
})
export class AppRemoteRequest {
  @Element() el: HTMLElement;

  @Prop()
  fullscreen: boolean = false;

  @State()
  private fullscreenEnable: boolean = false;

  @Event()
  toggleFullScreen: EventEmitter<void>;

  private remoteService: RemoteService;

  constructor() {
    this.remoteService = RemoteService.getInstance();
  }

  componentWillLoad() {
    this.fullscreenEnable = !isIPad() && !isMobile();
  }

  private async closePopover(action?: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      ...(action && {action}),
    });
  }

  private async toggleFullScreenMode() {
    this.toggleFullScreen.emit();
    await this.closePopover();
  }

  private async openPresenter() {
    const room: string | null = await this.remoteService.getRoom();

    if (!room || room === '') {
      return;
    }

    window?.open(`https://deckdeckgo.app/remote/${room}`, '_blank');

    await this.closePopover();
  }

  private async toggleRemoteEnabled() {
    await this.remoteService.switch(!remoteStore.state.remote);
  }

  render() {
    return (
      <div class="ion-padding">
        {this.renderFullscreen()}

        {this.renderRemoteControl()}

        {this.renderPresenterView()}

        <ion-list>
          <ion-item>
            {this.renderLabel()}
            <ion-toggle slot="end" mode="md" color="dark" checked={remoteStore.state.remote} onIonChange={() => this.toggleRemoteEnabled()}></ion-toggle>
          </ion-item>
        </ion-list>
      </div>
    );
  }

  private renderLabel() {
    if (remoteStore.state.remote) {
      return (
        <ion-label>
          <small>Remote control is enabled</small>
        </ion-label>
      );
    } else {
      return (
        <ion-label>
          <small>Remote control is disabled</small>
        </ion-label>
      );
    }
  }

  private renderPresenterView() {
    return (
      <Fragment>
        <p class={remoteStore.state.remote ? 'title' : 'title disabled'}>
          <strong>Presenter view</strong>
        </p>
        <p class={remoteStore.state.remote ? '' : 'disabled'}>Use all the remote features on this device with a control panel.</p>

        <ion-button
          type="submit"
          color="medium"
          size="small"
          shape="round"
          class="ion-margin-bottom"
          disabled={!remoteStore.state.remote}
          onClick={() => this.openPresenter()}>
          <ion-label>Open presenter view</ion-label>
        </ion-button>
      </Fragment>
    );
  }

  private renderRemoteControl() {
    return (
      <Fragment>
        <p class={remoteStore.state.remote ? 'title' : 'title disabled'}>
          <strong>Remote control</strong>
        </p>
        <p class={remoteStore.state.remote ? '' : 'disabled'}>
          Control your presentation from your phone or tablet where you can also see your notes, set a timer and draw over your slides.
        </p>

        <ion-button
          type="submit"
          color="medium"
          size="small"
          shape="round"
          disabled={!remoteStore.state.remote}
          onClick={() => this.closePopover(MoreAction.REMOTE)}>
          <ion-label>Connect</ion-label>
        </ion-button>
      </Fragment>
    );
  }

  private renderFullscreen() {
    if (!this.fullscreenEnable || this.fullscreen) {
      return undefined;
    }

    return (
      <Fragment>
        <p class="title">
          <strong>Start presenting</strong>
        </p>
        <p>Enter fullscreen mode to start presenting your slides.</p>

        <ion-button type="submit" color="primary" size="small" shape="round" onClick={() => this.toggleFullScreenMode()}>
          <ion-label>Enter fullscreen</ion-label>
        </ion-button>
      </Fragment>
    );
  }
}
