import {Component, Element, h, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import {RemoteService} from '../../../../services/editor/remote/remote.service';

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
    this.remoteService
      .watchRequests()
      .pipe(take(1))
      .subscribe((requests: DeckdeckgoEventDeckRequest[] | undefined) => {
        this.request = requests && requests.length >= 1 ? requests[0] : undefined;
      });
  }

  render() {
    return (
      <div class="ion-padding">
        <p>No pending requests.</p>
      </div>
    );
  }
}
