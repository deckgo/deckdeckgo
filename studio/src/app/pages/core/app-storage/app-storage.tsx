import {Component, h, Fragment, State, ComponentInterface} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';

import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';

import {ImageEvents} from '../../../events/core/image/image.events';

@Component({
  tag: 'app-storage',
  styleUrl: 'app-storage.scss'
})
export class AppStorage implements ComponentInterface {
  @State()
  private loading: boolean = true;

  @State()
  private folder: 'data' | 'images' = 'images';

  private readonly debounceLoading: () => void;

  private imageEvents: ImageEvents = new ImageEvents();

  constructor() {
    this.debounceLoading = debounce(() => (this.loading = false), 750);
  }

  async componentWillLoad() {
    await this.imageEvents.init();

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
        <h1>{i18n.state.menu.assets}</h1>

        {authStore.state.anonymous ? this.renderNotLoggedInContent() : this.renderGuardedContent()}
      </main>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_dashboard, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  }

  private renderGuardedContent() {
    return (
      <Fragment>
        {this.renderFilter()}

        <app-assets folder={this.folder}></app-assets>
      </Fragment>
    );
  }

  private renderFilter() {
    return (
      <div class="select">
        <ion-select
          value={'images'}
          placeholder={i18n.state.editor.list}
          onIonChange={($event: CustomEvent) => (this.folder = $event.detail.value)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end">
          <ion-select-option value="images">Images</ion-select-option>
          <ion-select-option value="data">Data</ion-select-option>
        </ion-select>
      </div>
    );
  }
}
