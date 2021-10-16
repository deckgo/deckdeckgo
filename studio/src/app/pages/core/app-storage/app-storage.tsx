import {Component, h, Fragment, State, ComponentInterface} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';

import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';
import {debounce} from '@deckdeckgo/utils';

@Component({
  tag: 'app-storage',
  styleUrl: 'app-storage.scss'
})
export class AppStorage implements ComponentInterface {
  @State()
  private loading: boolean = true;

  private readonly debounceLoading: () => void;

  constructor() {
    this.debounceLoading = debounce(() => (this.loading = false), 750);
  }

  componentWillLoad() {
    this.debounceLoading();
  }

  render() {
    return (
      <Fragment>
        <app-navigation></app-navigation>
        <ion-content class="ion-padding">{this.renderContent()}</ion-content>
      </Fragment>
    );
  }

  private renderContent() {
    if (this.loading) {
      return <app-spinner></app-spinner>;
    }

    return (
      <main class="ion-padding fit">
        <h1>{i18n.state.menu.storage}</h1>

        {authStore.state.anonymous ? this.renderNotLoggedInContent() : this.renderGuardedContent()}
      </main>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_storage, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  }

  private renderGuardedContent() {
    return <div>TODO</div>;
  }
}
