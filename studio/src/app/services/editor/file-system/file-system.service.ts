import JSZip from 'jszip';

import {validate as uuidValidate, v4 as uuid} from 'uuid';

import {get, getMany} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';
import docStore from '../../../stores/doc.store';
import offlineStore from '../../../stores/offline.store';

import {Deck, Slide, FileImportData, UserAsset, Doc, Paragraph} from '@deckdeckgo/editor';

import {ImportAsset, ImportData, importEditorAssets, importEditorData, importEditorSync} from '../../../utils/editor/import.utils';
import {
  getDeckBackgroundImage,
  getParagraphsLocalImages,
  getParagraphsOnlineImages,
  getSlidesLocalCharts,
  getSlidesLocalImages,
  getSlidesOnlineCharts,
  getSlidesOnlineImages
} from '../../../utils/editor/assets.utils';

export class FileSystemService {
  private static instance: FileSystemService;

  static getInstance(): FileSystemService {
    if (!FileSystemService.instance) {
      FileSystemService.instance = new FileSystemService();
    }
    return FileSystemService.instance;
  }

  async importData(file: File): Promise<'doc' | 'deck'> {
    const {data, assets, sync} = await this.unzip(file);

    await importEditorAssets(assets);
    await importEditorData(data);

    const {doc} = data;

    const result: 'doc' | 'deck' = doc !== undefined ? 'doc' : 'deck';

    if (!sync) {
      return result;
    }

    await importEditorSync(data);

    return result;
  }

  async exportData() {
    if (!this.isDeckEdited() && !this.isDocEdited()) {
      throw new Error('No deck or doc found');
    }

    const deck: Deck | undefined = await this.getDeck();
    const slides: Slide[] | undefined = await this.getSlides();

    const doc: Doc | undefined = await this.getDoc();
    const paragraphs: Paragraph[] | undefined = await this.getParagraphs();

    const localSlidesImages: UserAsset[] = await getSlidesLocalImages({deck: deckStore.state.deck});
    const onlineSlidesImages: UserAsset[] = await getSlidesOnlineImages({deck: deckStore.state.deck});

    const localSlidesCharts: UserAsset[] = await getSlidesLocalCharts({deck: deckStore.state.deck});
    const onlineSlidesCharts: UserAsset[] = await getSlidesOnlineCharts({deck: deckStore.state.deck});

    const deckBackground: UserAsset | undefined = await getDeckBackgroundImage();

    const localParagraphsImages: UserAsset[] = await getParagraphsLocalImages({doc: docStore.state.doc});
    const onlineParagraphsImages: UserAsset[] = await getParagraphsOnlineImages({doc: docStore.state.doc});

    const blob: Blob = await this.zip({
      data: {
        id: deck?.id || doc?.id,
        ...(deck && {deck}),
        ...(slides && {slides}),
        ...(doc && {doc}),
        ...(paragraphs && {paragraphs})
      },
      assets: [
        ...localSlidesImages,
        ...onlineSlidesImages,
        ...localSlidesCharts,
        ...onlineSlidesCharts,
        ...(deckBackground ? [deckBackground] : []),
        ...localParagraphsImages,
        ...onlineParagraphsImages
      ]
    });

    await this.save({
      filename: deckStore.state.deck?.data?.name || docStore.state.doc?.data?.name,
      blob
    });
  }

  private isDeckEdited(): boolean {
    return !deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data;
  }

  private isDocEdited(): boolean {
    return !docStore.state.doc || !docStore.state.doc.id || !docStore.state.doc.data;
  }

  private async getDeck(): Promise<Deck | undefined> {
    if (!deckStore.state.deck) {
      return undefined;
    }

    const {id: deckId}: Deck = deckStore.state.deck;

    const deck: Deck | undefined = await get(`/decks/${deckId}`);

    if (!deck) {
      throw new Error('No deck found in IDB');
    }

    delete deck.data.deploy;
    delete deck.data.github;

    return deck;
  }

  private async getSlides(): Promise<Slide[] | undefined> {
    if (!deckStore.state.deck) {
      return undefined;
    }

    const deck: Deck = deckStore.state.deck;

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

  private async getDoc(): Promise<Doc | undefined> {
    if (!docStore.state.doc) {
      return undefined;
    }

    const {id: docId}: Doc = docStore.state.doc;

    const doc: Doc | undefined = await get(`/docs/${docId}`);

    if (!doc) {
      throw new Error('No doc found in IDB');
    }

    return doc;
  }

  private async getParagraphs(): Promise<Paragraph[] | undefined> {
    if (!docStore.state.doc) {
      return undefined;
    }

    const doc: Doc = docStore.state.doc;

    if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
      return [];
    }

    try {
      const keys: string[] = doc.data.paragraphs.map((paragraphId: string) => `/docs/${doc.id}/paragraphs/${paragraphId}`);
      return getMany<Paragraph>(keys);
    } catch (err) {
      throw new Error('Error while fetching paragraphs');
    }
  }

  private save({filename, blob}: {filename: string | undefined; blob: Blob}): Promise<void> {
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

  private async exportDownload({filename, blob}: {filename: string | undefined; blob: Blob}) {
    const a: HTMLAnchorElement = document.createElement('a');
    a.style.display = 'none';
    document.body.appendChild(a);

    const url: string = window.URL.createObjectURL(blob);

    a.href = url;
    a.download = `${filename || 'export'}.ddg`;

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

    zip.file('data.json', blob, {
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
    sync: boolean;
  }> {
    const zip = new JSZip();

    const content: JSZip = await zip.loadAsync(file);

    const data: FileImportData = await this.parseImportData(content);

    const zippedAssets: {path: string; file: JSZip.JSZipObject}[] = [];

    this.listZipAssets({content, zippedAssets, subPath: '/assets/local/images/'});
    this.listZipAssets({content, zippedAssets, subPath: '/assets/local/data/'});

    // We import the cloud assets only if user is online otherwise it will be possible to display those
    if (!offlineStore.state.online) {
      this.listZipAssets({content, zippedAssets, subPath: '/assets/online/images/'});
      this.listZipAssets({content, zippedAssets, subPath: '/assets/online/data/'});
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
      ...this.syncImportData(data),
      assets
    };
  }

  private listZipAssets({
    content,
    zippedAssets,
    subPath
  }: {
    content: JSZip;
    subPath: string;
    zippedAssets: {path: string; file: JSZip.JSZipObject}[];
  }) {
    content.folder(subPath).forEach((filename: string, file: JSZip.JSZipObject) =>
      zippedAssets.push({
        path: `${subPath}${filename}`,
        file
      })
    );
  }

  private async parseImportData(content: JSZip): Promise<FileImportData> {
    let data: string = await content.file('data.json').async('text');

    // If user is offline, then we load the online content saved in the cloud locally too, better display the content than none
    if (!offlineStore.state.online) {
      const assetsContent: string | null = await content.file('assets.json')?.async('text');
      const assets: UserAsset[] = assetsContent ? JSON.parse(assetsContent) : [];

      assets
        .filter(({url}) => url !== undefined)
        .forEach(({url, key}: UserAsset) => {
          // deckgo-img img-src="" and slide src=""
          data = data.replaceAll(`src=\\"${url}\\"`, `src=\\"${key}\\"`);
          data = data.replaceAll(`src=\\"${url.replaceAll('&', '&amp;')}\\"`, `src=\\"${key}\\"`);
        });
    }

    return JSON.parse(data);
  }

  /**
   * If data are exported from the editor, they contain ids, therefore imported without any further work.
   * In case they are imported from other source, such as Figma, these are new data.
   * They need to be imported with new ids and need to be added to the list of data to sync.
   * @param id
   * @param deck
   * @param slides
   * @param doc
   * @param paragraphs
   * @private
   */
  private syncImportData({id, deck, slides, doc, paragraphs}: FileImportData): {
    data: ImportData;
    sync: boolean;
  } {
    if (uuidValidate(id)) {
      return {
        data: {
          id,
          ...(deck && {deck: deck as Deck}),
          ...(slides && {slides: slides as Slide[]}),
          ...(doc && {doc: doc as Doc}),
          ...(paragraphs && {paragraphs: paragraphs as Paragraph[]})
        },
        sync: false
      };
    }

    // Generate ids for new deck and slides (Figma import)

    const deckId: string = uuid();
    const now: Date = new Date();

    const newSlides: Slide[] = slides.map((slide: Partial<Slide>) => ({
      data: {
        ...slide.data,
        updated_at: now,
        created_at: now
      },
      id: uuid()
    })) as Slide[];

    const newDeck: Deck = {
      data: {
        ...deck.data,
        slides: newSlides.map(({id}: Slide) => id),
        updated_at: now,
        created_at: now
      },
      id: deckId
    } as Deck;

    return {
      data: {
        id: deckId,
        deck: newDeck,
        slides: newSlides
      },
      sync: true
    };
  }
}
