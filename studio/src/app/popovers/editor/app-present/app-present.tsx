import {Component, Element, EventEmitter, h, Prop, State, Event, Fragment, Host} from '@stencil/core';

import {isIPad, isMobile} from '@deckdeckgo/utils';

import remoteStore from '../../../stores/remote.store';
import i18n from '../../../stores/i18n.store';

import {MoreAction} from '../../../types/editor/more-action';

import {RemoteService} from '../../../services/editor/remote/remote.service';

@Component({
  tag: 'app-present',
  styleUrl: 'app-present.scss'
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
      ...(action && {action})
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
      <Host>
        {this.renderFullscreen()}

        {this.renderRemoteControl()}

        {this.renderPresenterView()}

        <ion-list>
          <ion-item>
            {this.renderLabel()}
            <ion-toggle
              slot="end"
              mode="md"
              color="dark"
              checked={remoteStore.state.remote}
              onIonChange={() => this.toggleRemoteEnabled()}
            ></ion-toggle>
          </ion-item>
        </ion-list>
      </Host>
    );
  }

  private renderLabel() {
    if (remoteStore.state.remote) {
      return (
        <ion-label>
          <small>{i18n.state.editor.remote_enabled}</small>
        </ion-label>
      );
    } else {
      return (
        <ion-label>
          <small>{i18n.state.editor.remote_disabled}</small>
        </ion-label>
      );
    }
  }

  private renderPresenterView() {
    return (
      <Fragment>
        <p class={remoteStore.state.remote ? 'title' : 'title disabled'}>
          <strong>{i18n.state.editor.presenter_view}</strong>
        </p>
        <p class={remoteStore.state.remote ? '' : 'disabled'}>{i18n.state.editor.remote_features}</p>

        <ion-button
          type="submit"
          color="medium"
          size="small"
          shape="round"
          class="ion-margin-bottom"
          disabled={!remoteStore.state.remote}
          onClick={() => this.openPresenter()}
        >
          <ion-label>{i18n.state.editor.open_presenter_view}</ion-label>
        </ion-button>
      </Fragment>
    );
  }

  private renderRemoteControl() {
    return (
      <Fragment>
        <p class={remoteStore.state.remote ? 'title' : 'title disabled'}>
          <strong>{i18n.state.menu.remote_control}</strong>
        </p>
        <p class={remoteStore.state.remote ? '' : 'disabled'}>{i18n.state.editor.control_presentation}</p>

        <ion-button
          type="submit"
          color="medium"
          size="small"
          shape="round"
          disabled={!remoteStore.state.remote}
          onClick={() => this.closePopover(MoreAction.REMOTE)}
        >
          <ion-label>{i18n.state.editor.connect}</ion-label>
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
          <strong>{i18n.state.editor.start_presenting}</strong>
        </p>
        <p>{i18n.state.editor.enter_fullscreen_to_start}</p>

        <ion-button type="submit" color="primary" size="small" shape="round" onClick={() => this.toggleFullScreenMode()}>
          <ion-label>{i18n.state.editor.enter_fullscreen}</ion-label>
        </ion-button>
      </Fragment>
    );
  }
}
