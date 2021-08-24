import {get, getMany} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {ImportData, importEditorData} from '../../../utils/editor/import.utils';

import {SwService} from '../sw/sw.service';

export class FileSystemService {
  private static instance: FileSystemService;

  static getInstance(): FileSystemService {
    if (!FileSystemService.instance) {
      FileSystemService.instance = new FileSystemService();
    }
    return FileSystemService.instance;
  }

  async importData(file: File) {
    const data: ImportData = JSON.parse(await file.text());

    await importEditorData(data);

    // We try to cache the data so that the user can go offline with it asap if wished
    await SwService.getInstance().cacheServiceWorker();
  }

  async exportData() {
    if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
      throw new Error('No deck found');
    }

    const deck: Deck = await this.getDeck(deckStore.state.deck);
    const slides: Slide[] = await this.getSlides(deckStore.state.deck);

    await this.save({
      id: deck.id,
      deck,
      slides
    });
  }

  private async getDeck({id: deckId}: Deck): Promise<Deck> {
    const deck: Deck | undefined = await get(`/decks/${deckId}`);

    if (!deck) {
      throw new Error('No deck found in IDB');
    }

    delete deck.data.deploy;
    delete deck.data.github;
    delete deck.data.clone;

    return deck;
  }

  private async getSlides(deck: Deck): Promise<Slide[]> {
    if (!deck.data.slides || deck.data.slides.length <= 0) {
      return [];
    }

    try {
      const keys: string[] = deck.data.slides.map((slideId: string) => `/decks/${deck.id}/slides/${slideId}`);
      return getMany<Slide>(keys);
    } catch (err) {
      throw new Error('Error while fetching slides');
    }
  }

  private save(deck: ImportData): Promise<void> {
    if ('showSaveFilePicker' in window) {
      return this.exportNativeFileSystem(deck);
    }

    return this.exportDownload(deck);
  }

  private async exportNativeFileSystem(deck: ImportData) {
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

  private async exportDownload(deckSave: ImportData) {
    const a: HTMLAnchorElement = document.createElement('a');
    a.style.display = 'none';
    document.body.appendChild(a);

    const blob: Blob = new Blob([JSON.stringify(deckSave)], {type: 'octet/stream'});
    const url: string = window.URL.createObjectURL(blob);

    a.href = url;
    a.download = `${deckSave.deck.data.name}.json`;

    a.click();

    window.URL.revokeObjectURL(url);

    if (a && a.parentElement) {
      a.parentElement.removeChild(a);
    }
  }
}
