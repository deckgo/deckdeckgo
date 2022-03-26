import {ImageLoadEvents} from '@deckdeckgo/sync';
import {debounce} from '@deckdeckgo/utils';
import {Component, ComponentInterface, Fragment, h, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';
import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';

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

  private imageEvents: ImageLoadEvents = new ImageLoadEvents();

  private storageFilesRef: HTMLAppStorageFilesElement | undefined;

  constructor() {
    this.debounceLoading = debounce(() => (this.loading = false), 750);
  }

  componentWillLoad() {
    this.imageEvents.init();

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

        {!authStore.state.authUser ? this.renderNotLoggedInContent() : this.renderGuardedContent()}
      </main>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_assets, {
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
        <ion-label>{i18n.state.settings.info_assets}</ion-label>

        {this.renderFilter()}

        <app-storage-files
          class="ion-padding-top ion-padding-bottom"
          folder={this.folder}
          admin={true}
          ref={(el) => (this.storageFilesRef = el as HTMLAppStorageFilesElement)}></app-storage-files>
      </Fragment>
    );
  }

  private renderFilter() {
    return (
      <div class="filter">
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

        <button class="ion-activatable refresh" onClick={async () => await this.storageFilesRef?.resetAndSearch()}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="sync" ariaLabel={i18n.state.core.close}></AppIcon>
        </button>
      </div>
    );
  }
}
