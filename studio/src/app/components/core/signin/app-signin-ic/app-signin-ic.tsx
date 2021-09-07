import {Component, Event, EventEmitter, Fragment, h, State} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {AppIcon} from '../../app-icon/app-icon';

import {AuthService} from '../../../../services/auth/auth.service';
import {AuthFactoryService} from '../../../../services/auth/auth.factory.service';

@Component({
  tag: 'app-signin-ic',
  styleUrl: 'app-signin-ic.scss'
})
export class AppSignInIc {
  @Event()
  inProgress: EventEmitter<boolean>;

  @State()
  private signInInProgress: boolean = false;

  private readonly authService: AuthService;

  constructor() {
    this.authService = AuthFactoryService.getInstance();
  }

  private async signIn() {
    this.inProgress.emit(true);
    this.signInInProgress = true;

    await this.authService.signIn();
  }

  render() {
    return (
      <Fragment>
        {this.renderMsg()}

        <div class="actions">{this.renderAction()}</div>

        {this.renderTerms()}
      </Fragment>
    );
  }

  private renderAction() {
    if (this.signInInProgress) {
      return <ion-spinner color="medium"></ion-spinner>;
    }

    return (
      <ion-button shape="round" color="dark" onClick={async () => await this.signIn()}>
        <AppIcon name="dfinity" path="icons" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.sign_in.internet_identity}</ion-label>
      </ion-button>
    );
  }

  private renderMsg() {
    return [
      <h1 class="ion-text-center ion-padding-start ion-padding-end">{i18n.state.sign_in.hi}</h1>,
      <p class="ion-text-center ion-padding">{i18n.state.sign_in.why_internet_computer}</p>
    ];
  }

  private renderTerms() {
    return (
      <p class="ion-text-center ion-margin terms">
        By continuing, you are indicating that you accept our{' '}
        <a href="https://deckdeckgo.com/terms" rel="noopener norefferer" target="_blank">
          <ion-label>{i18n.state.links.terms_of_use}</ion-label>
        </a>{' '}
        and{' '}
        <a href="https://deckdeckgo.com/privacy" rel="noopener norefferer" target="_blank">
          <ion-label>{i18n.state.links.privacy_policy}</ion-label>
        </a>
        .
      </p>
    );
  }
}
