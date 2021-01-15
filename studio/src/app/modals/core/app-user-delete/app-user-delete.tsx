import {Component, Element, Listen, Prop, State, h} from '@stencil/core';

import {UserUtils} from '../../../utils/core/user.utils';

@Component({
  tag: 'app-user-delete',
  styleUrl: 'app-user-delete.scss',
})
export class AppUserDelete {
  @Element() el: HTMLElement;

  @State()
  private valid: boolean = false;

  @Prop()
  username: string;

  private inputUsername: string;

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

  private handleUsernameInput($event: CustomEvent<KeyboardEvent>) {
    this.inputUsername = ($event.target as InputTargetEvent).value;
  }

  private validateUsernameInput() {
    this.valid = UserUtils.validUsername(this.username) && this.username === this.inputUsername;
  }

  private async handleSubmit(e: Event) {
    if (!this.valid) {
      return;
    }

    e.preventDefault();

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(true);
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="danger">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Are you absolutely sure?</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <p>
          This action cannot be undone. This will permanently delete your user <strong>{this.username}</strong>.
        </p>

        <form onSubmit={(e: Event) => this.handleSubmit(e)}>
          <p class="ion-no-margin">Please type your username to confirm.</p>

          <ion-item>
            <ion-input
              debounce={300}
              required={true}
              input-mode="text"
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleUsernameInput(e)}
              onIonChange={() => this.validateUsernameInput()}></ion-input>
          </ion-item>

          <ion-button type="submit" disabled={!this.valid} color="danger" class="ion-margin-top" shape="round">
            <ion-label>I understand, delete my user</ion-label>
          </ion-button>
        </form>

        <app-unpublish></app-unpublish>
      </ion-content>,
    ];
  }
}
