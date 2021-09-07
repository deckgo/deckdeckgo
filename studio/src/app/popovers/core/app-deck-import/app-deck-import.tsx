import {Component, Element, Fragment, h, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';
import authStore from '../../../stores/auth.store';

import {Deck} from '../../../models/data/deck';

import {DeckImportService} from '../../../services/deck/deck-import.service';
import {getOnlineStorageService, StorageService} from '../../../services/storage/storage.service';

import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';

@Component({
  tag: 'app-deck-import',
  styleUrl: 'app-deck-import.scss'
})
export class AppDeckImport {
  @Element() el: HTMLElement;

  private uploadInput!: HTMLInputElement;

  @State()
  private progress: 'uploading' | 'initializing' | 'done' | undefined = undefined;

  private storageOnlineService: StorageService;
  private deckImportService: DeckImportService;

  constructor() {
    this.storageOnlineService = getOnlineStorageService();
    this.deckImportService = DeckImportService.getInstance();
  }

  async closePopover(deckId?: string) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({deckId});
  }

  private openFilePicker() {
    this.uploadInput?.click();
  }

  private async upload() {
    if (!this.uploadInput) {
      this.progress = undefined;
      return;
    }

    if (this.uploadInput.files?.length > 0) {
      try {
        await this.uploadFile(this.uploadInput.files[0]);
      } catch (err) {
        errorStore.state.error = 'Data cannot be uploaded.';
      }
    }
  }

  private async uploadFile(file: File) {
    this.progress = 'uploading';

    await this.storageOnlineService.uploadFile(file, `decks`, 20971520, false);

    this.progress = 'initializing';

    await this.deckImportService.snapshot(authStore.state.authUser?.uid, this.watchCreatedDeck);
  }

  private watchCreatedDeck = async (deck: Deck, unsubscribe) => {
    if (!deck || !deck.data) {
      return;
    }

    await this.closePopover(deck.id);

    this.progress = 'done';

    unsubscribe();
  };

  private async navigateSignIn() {
    await this.closePopover();

    signIn();
  }

  render() {
    return (
      <div class="ion-padding">
        <h2>{i18n.state.import.import_from_figma}</h2>

        <p>
          {renderI18n(i18n.state.import.create, {
            placeholder: '{0}',
            value: (
              <a href="https://figma.com" target="_blank" rel="noopener noreferrer">
                Figma
              </a>
            )
          })}
        </p>

        {authStore.state.loggedIn ? this.renderImport() : this.renderSignIn()}
      </div>
    );
  }

  private renderSignIn() {
    return (
      <Fragment>
        <p>{i18n.state.import.sign_in}</p>

        <div class="actions">
          <ion-button onClick={async () => this.navigateSignIn()} shape="round" color="light" size="small">
            <ion-label>{i18n.state.nav.sign_in}</ion-label>
          </ion-button>
        </div>
      </Fragment>
    );
  }

  private renderImport() {
    return (
      <Fragment>
        <p>
          {renderI18n(i18n.state.import.export, {
            placeholder: '{0}',
            value: (
              <a href="https://www.figma.com/community/plugin/950777256486678678/Figma-to-DeckDeckGo" target="_blank" rel="noopener noreferrer">
                plugin
              </a>
            )
          })}
        </p>

        {this.renderActions()}

        <input type="file" accept=".zip" onChange={() => this.upload()} ref={(el) => (this.uploadInput = el as HTMLInputElement)} />
      </Fragment>
    );
  }

  private renderActions() {
    if (this.progress !== undefined) {
      return (
        <div class="in-progress">
          <ion-progress-bar type="indeterminate" color="primary"></ion-progress-bar>
          <ion-label>{this.renderProgressMsg()}</ion-label>
        </div>
      );
    }

    return (
      <div class="actions">
        <ion-button onClick={() => this.openFilePicker()} shape="round" color="primary" size="small" disabled={this.progress !== undefined}>
          <ion-label>{i18n.state.import.import}</ion-label>
        </ion-button>
      </div>
    );
  }

  private renderProgressMsg() {
    if (this.progress === 'uploading') {
      return i18n.state.import.upload;
    } else if (this.progress === 'initializing') {
      return i18n.state.import.hang_on_creation;
    } else {
      return i18n.state.import.done;
    }
  }
}
