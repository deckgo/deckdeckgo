import JSZip from 'jszip';

import {get, getMany} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';
import offlineStore from '../../../stores/offline.store';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {ImportAsset, ImportData, importEditorAssets, importEditorData} from '../../../utils/editor/import.utils';
import {
  getDeckLocalImage,
  getSlidesLocalCharts,
  getSlidesLocalImages,
  getSlidesOnlineCharts,
  getSlidesOnlineImages,
  UserAsset
} from '../../../utils/editor/assets.utils';

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
    const {data, assets} = await this.unzip(file);

    await importEditorAssets(assets);
    await importEditorData(data);

    // TODO: Remove unuseful
    // We try to cache the data so that the user can go offline with it asap if wished
    await SwService.getInstance().cacheServiceWorker();
  }

  async exportData() {
    if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
      throw new Error('No deck found');
    }

    const deck: Deck = await this.getDeck(deckStore.state.deck);
    const slides: Slide[] = await this.getSlides(deckStore.state.deck);

    const localImages: UserAsset[] = await getSlidesLocalImages({deck: deckStore.state.deck});
    const localCharts: UserAsset[] = await getSlidesLocalCharts({deck: deckStore.state.deck});

    const onlineImages: UserAsset[] = await getSlidesOnlineImages({deck: deckStore.state.deck});
    const onlineCharts: UserAsset[] = await getSlidesOnlineCharts({deck: deckStore.state.deck});

    const deckBackground: UserAsset | undefined = await getDeckLocalImage();
    // TODO deck background online

    const blob: Blob = await this.zip({
      data: {
        id: deck.id,
        deck,
        slides
      },
      assets: [...localImages, ...localCharts, ...onlineImages, ...onlineCharts, ...(deckBackground ? [deckBackground] : [])]
    });

    await this.save({
      filename: deckStore.state.deck.data.name,
      blob
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

  private save({filename, blob}: {filename: string; blob: Blob}): Promise<void> {
    if ('showSaveFilePicker' in window) {
      return this.exportNativeFileSystem(blob);
    }

    return this.exportDownload({filename, blob});
  }

  private async exportNativeFileSystem(blob: Blob) {
    const fileHandle: FileSystemFileHandle = await this.getNewFileHandle();

    if (!fileHandle) {
      throw new Error('Cannot access filesystem');
    }

    await this.writeFile(fileHandle, blob);
  }

  private async getNewFileHandle(): Promise<FileSystemFileHandle> {
    const opts: SaveFilePickerOptions = {
      types: [
        {
          description: 'DeckDeckGo Files',
          accept: {
            'application/octet-stream': ['.ddg']
          }
        }
      ]
    };

    return showSaveFilePicker(opts);
  }

  private async writeFile(fileHandle: FileSystemFileHandle, blob: Blob) {
    const writer = await fileHandle.createWritable();
    await writer.write(blob);
    await writer.close();
  }

  private async exportDownload({filename, blob}: {filename: string; blob: Blob}) {
    const a: HTMLAnchorElement = document.createElement('a');
    a.style.display = 'none';
    document.body.appendChild(a);

    const url: string = window.URL.createObjectURL(blob);

    a.href = url;
    a.download = `${filename}.ddg`;

    a.click();

    window.URL.revokeObjectURL(url);

    if (a && a.parentElement) {
      a.parentElement.removeChild(a);
    }
  }

  private async zip({data, assets}: {data: ImportData; assets: UserAsset[]}): Promise<Blob> {
    const zip = new JSZip();

    assets.forEach(({key, blob}: UserAsset) =>
      zip.file(key, blob, {
        base64: true
      })
    );

    const blob: Blob = new Blob([JSON.stringify(data)], {type: 'application/json'});

    zip.file('deck.json', blob, {
      base64: true
    });

    const assetsBlob: Blob = new Blob(
      [
        JSON.stringify(
          assets.map(({key, url}: UserAsset) => ({
            key,
            ...(url && {url})
          }))
        )
      ],
      {type: 'application/json'}
    );

    zip.file('assets.json', assetsBlob, {
      base64: true
    });

    return zip.generateAsync({type: 'blob'});
  }

  private async unzip(file: File): Promise<{
    data: ImportData;
    assets: ImportAsset[];
  }> {
    const zip = new JSZip();

    const content: JSZip = await zip.loadAsync(file);

    const data: ImportData = await this.parseImportData(content);

    const zippedAssets: {path: string; file: JSZip.JSZipObject}[] = [];

    content
      .folder('/assets/local/images/')
      .forEach((filename: string, file: JSZip.JSZipObject) => zippedAssets.push({path: `/assets/local/images/${filename}`, file}));
    content
      .folder('/assets/local/data/')
      .forEach((filename: string, file: JSZip.JSZipObject) => zippedAssets.push({path: `/assets/local/data/${filename}`, file}));

    // We import the cloud assets only if user is online otherwise it will be possible to display those
    if (!offlineStore.state.online) {
      content
        .folder('/assets/online/images/')
        .forEach((filename: string, file: JSZip.JSZipObject) => zippedAssets.push({path: `/assets/online/images/${filename}`, file}));
      content
        .folder('/assets/online/data/')
        .forEach((filename: string, file: JSZip.JSZipObject) => zippedAssets.push({path: `/assets/online/data/${filename}`, file}));
    }

    const promises: Promise<ImportAsset>[] = zippedAssets.map(
      ({path, file}: {path: string; file: JSZip.JSZipObject}) =>
        new Promise<ImportAsset>(async (resolve) => {
          const blob: Blob = await file.async('blob');

          resolve({
            path,
            blob
          });
        })
    );

    const assets: ImportAsset[] = await Promise.all(promises);

    return {
      data,
      assets
    };
  }

  private async parseImportData(content: JSZip): Promise<ImportData> {
    let deck: string = await content.file('deck.json').async('text');

    // If user is offline, then we load the online content saved in the cloud locally too, better display the content than none
    if (!offlineStore.state.online) {
      const assetsContent: string | null = await content.file('assets.json')?.async('text');
      const assets: UserAsset[] = assetsContent ? JSON.parse(assetsContent) : [];

      assets
        .filter(({url}) => url !== undefined)
        .forEach(({url, key}: UserAsset) => {
          // deckgo-img img-src="" and slide src=""
          deck = deck.replaceAll(`src=\\"${url}\\"`, `src=\\"${key}\\"`);
          deck = deck.replaceAll(`src=\\"${url.replaceAll('&', '&amp;')}\\"`, `src=\\"${key}\\"`);
        });
    }

    return JSON.parse(deck);
  }
}
