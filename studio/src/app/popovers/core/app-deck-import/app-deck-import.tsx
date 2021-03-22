import {Component, Element, h, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';

import {StorageService} from '../../../services/storage/storage.service';
import {renderI18n} from '../../../utils/core/i18n.utils';

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

  constructor() {
    this.storageService = StorageService.getInstance();
  }

  async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
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

    this.uploading = false;
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
      </div>
    );
  }
}
