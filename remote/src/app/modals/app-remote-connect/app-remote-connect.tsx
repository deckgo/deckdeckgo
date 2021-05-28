import {Component, Element, Listen, State, h} from '@stencil/core';

import remoteStore, {ActiveRoom} from '../../stores/remote.store';

// Services
import {CommunicationService} from '../../services/communication/communication.service';

@Component({
  tag: 'app-remote-connect',
  styleUrl: 'app-remote-connect.scss',
})
export class AppRemoteConnect {
  @Element() el: HTMLElement;

  @State()
  private readonly clientId: string;

  private communicationService: CommunicationService;

  constructor() {
    this.communicationService = CommunicationService.getInstance();

    this.clientId = this.communicationService.clientId;
  }

  async componentDidLoad() {
    await this.communicationService.connect();

    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackbutton(_e: PopStateEvent) {
    await this.closeModal(false);
  }

  async closeModal(connect: boolean) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(connect);
  }

  async updateAndCloseModal(room: string) {
    this.communicationService.room = room;

    await this.closeModal(true);
  }

  render() {
    return [
      <app-header>
        <ion-buttons slot="start">
          <ion-button onClick={() => this.closeModal(false)}>
            <ion-icon name="close"></ion-icon>
          </ion-button>
        </ion-buttons>
      </app-header>,

      <ion-content class="ion-padding">{this.renderContent()}</ion-content>,
    ];
  }

  private renderContent() {
    if (remoteStore.state.rooms && remoteStore.state.rooms.length > 0) {
      return (
        <ion-list>
          <ion-list-header class="ion-padding-bottom">
            <ion-label>
              Pick a presentation. Your remote ID is <strong>{this.clientId}</strong>.
            </ion-label>
          </ion-list-header>

          <ion-radio-group>{this.renderRooms()}</ion-radio-group>
        </ion-list>
      );
    } else {
      return <h1>No presentations found, try to refresh yours...</h1>;
    }
  }

  private renderRooms() {
    return remoteStore.state.rooms.map((activeRoom: ActiveRoom) => {
      return (
        <ion-item onClick={() => this.updateAndCloseModal(activeRoom.room)} disabled={activeRoom.connected}>
          <ion-label>{activeRoom.room}</ion-label>
          <ion-radio slot="end" value={activeRoom.room}></ion-radio>
        </ion-item>
      );
    });
  }
}
