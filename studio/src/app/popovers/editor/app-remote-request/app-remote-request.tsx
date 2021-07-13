import {Component, Element, h, State} from '@stencil/core';

import {DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import remoteStore from '../../../stores/remote.store';
import i18n from '../../../stores/i18n.store';

import {RemoteService} from '../../../services/editor/remote/remote.service';

import {renderI18n} from '../../../utils/core/i18n.utils';

import { AppIcon } from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-remote-request',
  styleUrl: 'app-remote-request.scss'
})
export class AppRemoteRequest {
  @Element() el: HTMLElement;

  @State()
  request: DeckdeckgoEventDeckRequest | undefined;

  private remoteService: RemoteService;

  constructor() {
    this.remoteService = RemoteService.getInstance();
  }

  async componentDidLoad() {
    this.request = remoteStore.state.pendingRequests?.length >= 1 ? remoteStore.state.pendingRequests[0] : undefined;
  }

  private async shiftRequestsAndClose() {
    await this.remoteService.shiftPendingRequests();

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async accept() {
    this.remoteService.acceptRequest(this.request);

    await this.shiftRequestsAndClose();
  }

  render() {
    return <div class="ion-padding">{this.renderRequest()}</div>;
  }

  private renderRequest() {
    if (this.request === undefined) {
      return <p>{i18n.state.editor.no_pending_requests}</p>;
    }

    return [
      <p>
        {renderI18n(i18n.state.editor.grant_access, {
          placeholder: '{0}',
          value: <strong>{this.request.message}</strong>
        })}
      </p>,

      <div class="actions">
        <button class="navigation ion-activatable transparent dismiss" onClick={() => this.shiftRequestsAndClose()} aria-label={i18n.state.editor.deny}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="close" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>

        <button class="navigation ion-activatable primary connect" onClick={() => this.accept()} aria-label={i18n.state.editor.accept}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="checkmark" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </div>
    ];
  }
}
