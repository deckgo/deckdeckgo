import {Component, ComponentInterface, h, Event, EventEmitter, State, Fragment, Prop} from '@stencil/core';

import {signIn} from '../../providers/auth/auth.ic';

@Component({
  tag: 'deckgo-ic-signin',
  styleUrl: 'ic-signin.scss'
})
export class IcSignin implements ComponentInterface {
  @Prop()
  i18n: Record<string, Record<string, string>>;

  @Prop()
  config: Record<string, string>;

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
        <ion-icon
          src={`${this.config.globalAssetsUrl}/icons/dfinity.svg`}
          aria-label=""
          aria-hidden={true}
          slot="start"
          lazy={true}></ion-icon>
        <ion-label>{this.i18n.sign_in.internet_identity}</ion-label>
      </ion-button>
    );
  }

  private renderTerms() {
    return (
      <p class="ion-text-center ion-margin terms">
        By continuing, you are indicating that you accept our{' '}
        <a href="https://deckdeckgo.com/terms" rel="noopener norefferer" target="_blank">
          <ion-label>{this.i18n.links.terms_of_use}</ion-label>
        </a>{' '}
        and{' '}
        <a href="https://deckdeckgo.com/privacy" rel="noopener norefferer" target="_blank">
          <ion-label>{this.i18n.links.privacy_policy}</ion-label>
        </a>
        .
      </p>
    );
  }
}
