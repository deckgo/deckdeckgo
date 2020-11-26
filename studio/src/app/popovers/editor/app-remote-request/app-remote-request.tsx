import {Component, Element, h, State} from '@stencil/core';

import remoteStore from '../../../stores/remote.store';

import {DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import {RemoteService} from '../../../services/editor/remote/remote.service';

@Component({
  tag: 'app-remote-request',
  styleUrl: 'app-remote-request.scss',
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
      return <p>No pending requests.</p>;
    }

    return [
      <p>
        Grant access to remote <strong>{this.request.message}</strong>?
      </p>,

      <div class="actions">
        <button class="navigation ion-activatable transparent dismiss" onClick={() => this.shiftRequestsAndClose()}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-label="Deny" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>

        <button class="navigation ion-activatable primary connect" onClick={() => this.accept()}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-label="Accept" src="/assets/icons/ionicons/checkmark.svg"></ion-icon>
        </button>
      </div>,
    ];
  }
}
