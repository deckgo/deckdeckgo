import {Component, ComponentInterface, Event, EventEmitter, h, Host, Prop, State} from '@stencil/core';
import {signIn} from '../../providers/auth/auth.ic';
import {IconDfinity} from '../icons/dfinity';

@Component({
  tag: 'deckgo-ic-signin',
  styleUrl: 'ic-signin.scss'
})
export class IcSignin implements ComponentInterface {
  @Prop()
  i18n: Record<string, Record<string, string>>;

  @Prop()
  config: Record<string, string>;

  @Prop()
  signInSuccess: () => void;

  @Prop()
  signInError: (err?: string) => void;

  @Event()
  inProgress: EventEmitter<boolean>;

  @State()
  private signInInProgress: boolean = false;

  @Event()
  ddgSignInSuccess: EventEmitter<void>;

  @Event()
  ddgSignInError: EventEmitter<string | undefined>;

  private async signUserIn() {
    this.inProgress.emit(true);
    this.signInInProgress = true;

    const signInSuccess: () => void = this.signInSuccess || (() => this.ddgSignInSuccess.emit());
    const signInError: (err?: string) => void = this.signInError || ((err?: string) => this.ddgSignInError.emit(err));

    await signIn({onSuccess: signInSuccess, onError: signInError});
  }

  render() {
    return (
      <Host>
        <div class="actions">
          {this.renderSpinner()}
          {this.renderAction()}
        </div>

        {this.renderTerms()}
      </Host>
    );
  }

  private renderSpinner() {
    return (
      <div class={`spinner ${!this.signInInProgress ? 'hidden' : ''}`}>
        <slot name="spinner" />
      </div>
    );
  }

  private renderAction() {
    if (this.signInInProgress) {
      return undefined;
    }

    return (
      <button onClick={async () => await this.signUserIn()}>
        <IconDfinity />
        {this.i18n?.sign_in.internet_identity}
      </button>
    );
  }

  private renderTerms() {
    const {terms, privacy} = this.config || {};

    return (
      <p class="terms">
        By continuing, you are indicating that you accept our{' '}
        <a href={terms} rel="noopener norefferer" target="_blank">
          {this.i18n?.links.terms_of_use}
        </a>{' '}
        and{' '}
        <a href={privacy} rel="noopener norefferer" target="_blank">
          {this.i18n?.links.privacy_policy}
        </a>
        .
      </p>
    );
  }
}
