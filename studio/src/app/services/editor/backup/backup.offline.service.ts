import firebase from 'firebase/app';

import {get} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';

import {Deck, DeckAttributes} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {FirestoreUtils} from '../../../utils/editor/firestore.utils';

interface DeckBackupData {
  name: string;

  attributes?: DeckAttributes;
  background?: string;
  header?: string;
  footer?: string;

  owner_id: string;

  slides?: Slide[];

  api_id?: string;

  created_at?: firebase.firestore.Timestamp | Date;
  updated_at?: firebase.firestore.Timestamp | Date;
}

interface DeckBackup {
  id: string;
  data: DeckBackupData;
}

export class BackupOfflineService {
  private static instance: BackupOfflineService;

  static getInstance(): BackupOfflineService {
    if (!BackupOfflineService.instance) {
      BackupOfflineService.instance = new BackupOfflineService();
    }
    return BackupOfflineService.instance;
  }

  async backup() {
    if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
      throw new Error('No deck found');
    }

    const slides: Slide[] = await this.getSlides(deckStore.state.deck);

    // We select what we want to backup and add the slides (not their id) in these data
    const backupDeckData: DeckBackupData = FirestoreUtils.filterDelete(this.prepareDeckBackupData(slides), true);

    await this.save({
      id: deckStore.state.deck.id,
      data: backupDeckData
    });
  }

  private prepareDeckBackupData(slides: Slide[]): DeckBackupData {
    return {
      name: deckStore.state.deck.data.name,

      attributes: deckStore.state.deck.data.attributes,
      background: deckStore.state.deck.data.background,
      header: deckStore.state.deck.data.header,
      footer: deckStore.state.deck.data.footer,

      owner_id: deckStore.state.deck.data.owner_id,

      slides,

      api_id: deckStore.state.deck.data.api_id,

      created_at: deckStore.state.deck.data.created_at,
      updated_at: deckStore.state.deck.data.updated_at
    };
  }

  private async getSlides(deck: Deck): Promise<Slide[]> {
    if (!deck.data.slides || deck.data.slides.length <= 0) {
      return [];
    }

    try {
      const promises: Promise<Slide>[] = [];

      for (let i: number = 0; i < deck.data.slides.length; i++) {
        const slideId: string = deck.data.slides[i];

        promises.push(get(`/decks/${deck.id}/slides/${slideId}`));
      }

      if (!promises || promises.length <= 0) {
        return [];
      }

      const slides: Slide[] = await Promise.all(promises);

      return slides;
    } catch (err) {
      throw new Error('Error while fetching slides');
    }
  }

  private save(deck: DeckBackup): Promise<void> {
    if ('showSaveFilePicker' in window) {
      return this.exportNativeFileSystem(deck);
    }

    return this.exportDownload(deck);
  }

  private async exportNativeFileSystem(deck: DeckBackup) {
    const fileHandle: FileSystemFileHandle = await this.getNewFileHandle();

    if (!fileHandle) {
      throw new Error('Cannot access filesystem');
    }

    await this.writeFile(fileHandle, JSON.stringify(deck));
  }

  private async getNewFileHandle(): Promise<FileSystemFileHandle> {
    const opts: SaveFilePickerOptions = {
      types: [
        {
          description: 'JSON Files',
          accept: {
            'application/json': ['.json']
          }
        }
      ]
    };

    return showSaveFilePicker(opts);
  }

  private async writeFile(fileHandle: FileSystemFileHandle, contents: string | BufferSource | Blob) {
    // Create a writer (request permission if necessary).
    const writer = await fileHandle.createWritable();
    // Write the full length of the contents
    await writer.write(contents);
    // Close the file and write the contents to disk
    await writer.close();
  }

  private async exportDownload(deck: DeckBackup) {
    const a: HTMLAnchorElement = document.createElement('a');
    a.style.display = 'none';
    document.body.appendChild(a);

    const blob: Blob = new Blob([JSON.stringify(deck)], {type: 'octet/stream'});
    const url: string = window.URL.createObjectURL(blob);

    a.href = url;
    a.download = `${deck.data.name}.json`;

    a.click();

    window.URL.revokeObjectURL(url);

    if (a && a.parentElement) {
      a.parentElement.removeChild(a);
    }
  }
}
