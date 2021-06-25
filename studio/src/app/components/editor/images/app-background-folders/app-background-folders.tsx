import {h, Component, State, Event, EventEmitter} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {StorageService} from '../../../../services/storage/storage.service';

@Component({
  tag: 'app-background-folders',
  styleUrl: 'app-background-folders.scss'
})
export class AppBackgroundFolders {
  @State()
  private currentFolder: string = 'images';

  @State()
  private folders: StorageFolder[];

  @Event()
  private selectFolder: EventEmitter<string>;

  private storageService: StorageService;

  constructor() {
    this.storageService = StorageService.getInstance();
  }

  async componentWillLoad() {
    await this.initFolders();
  }

  private async initFolders() {
    const storage: StorageFoldersList | undefined = await this.storageService.getFolders('backgrounds');

    const rootFolder: StorageFolder[] = [{name: 'images'}];

    if (!storage) {
      this.folders = [...rootFolder];
      return;
    }

    this.folders = [...rootFolder, ...storage.prefixes];
  }

  private openFolder(folder: StorageFolder) {
    this.currentFolder = folder.name;

    this.selectFolder.emit(this.currentFolder === 'images' ? 'images' : `backgrounds/${this.currentFolder}`);
  }

  render() {
    if (!this.folders || this.folders.length <= 1) {
      return undefined;
    }

    return <div class="folders">{this.renderFolders()}</div>;
  }

  private renderFolders() {
    return this.folders.map((folder: StorageFolder) => {
      const current: boolean = folder.name === this.currentFolder;

      return (
        <button class={`${current ? 'current' : ''}`} onClick={() => this.openFolder(folder)}>
          {this.renderFolderName(folder)}
          <ion-icon name={current ? 'folder-open' : 'folder'}></ion-icon>
        </button>
      );
    });
  }

  private renderFolderName(folder: StorageFolder) {
    if (folder.name === this.currentFolder) {
      return (
        <ion-label>
          <small>{folder.name === 'images' ? i18n.state.editor.your_images : i18n.state.editor.background}</small>
        </ion-label>
      );
    }

    return undefined;
  }
}
