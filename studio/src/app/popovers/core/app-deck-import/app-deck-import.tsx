import {Component, Element, Fragment, h, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';
import authStore from '../../../stores/auth.store';

import {Deck} from '../../../models/data/deck';

import {StorageService} from '../../../services/storage/storage.service';
import {DeckImportService} from '../../../services/deck/deck-import.service';

import {renderI18n} from '../../../utils/core/i18n.utils';
import {signIn} from '../../../utils/core/signin.utils';

@Component({
  tag: 'app-deck-import',
  styleUrl: 'app-deck-import.scss',
})
export class AppDeckImport {
  @Element() el: HTMLElement;

  private uploadInput!: HTMLInputElement;

  @State()
  private uploading: boolean = false;

  private storageService: StorageService;
  private deckImportService: DeckImportService;

  constructor() {
    this.storageService = StorageService.getInstance();
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
      this.uploading = false;
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
    this.uploading = true;

    await this.storageService.uploadFile(file, `decks`, 20971520, false);

    await this.deckImportService.snapshot(authStore.state.authUser?.uid, this.watchCreatedDeck);
  }

  private watchCreatedDeck = async (deck: Deck, unsubscribe) => {
    if (!deck || !deck.data) {
      return;
    }

    await this.closePopover(deck.id);

    this.uploading = false;

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
            ),
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
            ),
          })}
        </p>

        <div class="actions">
          <ion-button onClick={() => this.openFilePicker()} shape="round" color="primary" size="small" disabled={this.uploading}>
            <ion-label>{i18n.state.import.import}</ion-label>
          </ion-button>
        </div>

        <input type="file" accept=".zip" onChange={() => this.upload()} ref={(el) => (this.uploadInput = el as HTMLInputElement)} />
      </Fragment>
    );
  }
}
