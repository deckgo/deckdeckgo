import {Component, Element, EventEmitter, h, Prop, State, Event, Fragment} from '@stencil/core';

import {isIPad, isMobile} from '@deckdeckgo/utils';
import {MoreAction} from '../../../utils/editor/more-action';

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

  render() {
    return (
      <div class="ion-padding">
        {this.renderFullscreen()}

        {this.renderRemoteControl()}

        {this.renderPresenterView()}
      </div>
    );
  }

  private renderPresenterView() {
    return (
      <Fragment>
        <p class="title">
          <strong>Presenter view</strong>
        </p>
        <p>Use all the remote features on this device with a control panel.</p>

        <ion-button type="submit" color="medium" size="small" shape="round" class="ion-margin-bottom">
          <ion-label>Open presenter view</ion-label>
        </ion-button>
      </Fragment>
    );
  }

  private renderRemoteControl() {
    return (
      <Fragment>
        <p class="title">
          <strong>Remote control</strong>
        </p>
        <p>Control your presentation from your phone or tablet where you can also see your notes, set a timer and draw over your slides.</p>

        <ion-button type="submit" color="medium" size="small" shape="round" onClick={() => this.closePopover(MoreAction.REMOTE)}>
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
