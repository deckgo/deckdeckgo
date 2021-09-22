import {Component, Event, EventEmitter, Fragment, h, State} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {AppIcon} from '../../app-icon/app-icon';

import {signIn} from '../../../../providers/auth/auth.provider';

@Component({
  tag: 'app-signin-ic',
  styleUrl: 'app-signin-ic.scss'
})
export class AppSignInIc {
  @Event()
  inProgress: EventEmitter<boolean>;

  @State()
  private signInInProgress: boolean = false;

  private async signUserIn() {
    this.inProgress.emit(true);
    this.signInInProgress = true;

    await signIn();
  }

  render() {
    if (this.signInInProgress) {
      return <ion-spinner color="medium"></ion-spinner>;
    }

    return (
      <Fragment>
        <div class="actions">{this.renderAction()}</div>

        {this.renderTerms()}
      </Fragment>
    );
  }

  private renderAction() {
    return (
      <ion-button shape="round" color="dark" onClick={async () => await this.signUserIn()}>
        <AppIcon name="dfinity" path="icons" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.sign_in.internet_identity}</ion-label>
      </ion-button>
    );
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
