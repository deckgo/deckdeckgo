import {Deck, Doc, FileImportData, Paragraph, Slide, UserAsset} from '@deckdeckgo/editor';
import {offlineStore, editorStore} from '@deckdeckgo/studio';
import {get, getMany} from 'idb-keyval';
import JSZip from 'jszip';
import {v4 as uuid} from 'uuid';
import authStore from '../../../stores/auth.store';
import {
  getDeckBackgroundImage,
  getParagraphsLocalImages,
  getParagraphsOnlineImages,
  getSlidesLocalCharts,
  getSlidesLocalImages,
  getSlidesOnlineCharts,
  getSlidesOnlineImages
} from '../../../utils/editor/assets.utils';
import {ImportAsset, ImportData, importEditorAssets, importEditorData, importEditorSync} from '../../../utils/editor/import.utils';

export class FileSystemService {
  private static instance: FileSystemService;

  static getInstance(): FileSystemService {
    if (!FileSystemService.instance) {
      FileSystemService.instance = new FileSystemService();
    }
    return FileSystemService.instance;
  }

  async importData(file: File): Promise<'doc' | 'deck'> {
    const {data, assets} = await this.unzip(file);

    await importEditorAssets(assets);
    await importEditorData(data);

    const {doc} = data;

    const result: 'doc' | 'deck' = doc !== undefined ? 'doc' : 'deck';

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

    const localSlidesImages: UserAsset[] = await getSlidesLocalImages({deck: editorStore.default.state.deck});
    const onlineSlidesImages: UserAsset[] = await getSlidesOnlineImages({deck: editorStore.default.state.deck});

    const localSlidesCharts: UserAsset[] = await getSlidesLocalCharts({deck: editorStore.default.state.deck});
    const onlineSlidesCharts: UserAsset[] = await getSlidesOnlineCharts({deck: editorStore.default.state.deck});

    const deckBackground: UserAsset | undefined = await getDeckBackgroundImage();

    const localParagraphsImages: UserAsset[] = await getParagraphsLocalImages({doc: editorStore.default.state.doc});
    const onlineParagraphsImages: UserAsset[] = await getParagraphsOnlineImages({doc: editorStore.default.state.doc});

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
      filename: editorStore.default.state.deck?.data?.name || editorStore.default.state.doc?.data?.name,
      blob
    });
  }

  private isDeckEdited(): boolean {
    return !editorStore.default.state.deck || !editorStore.default.state.deck.id || !editorStore.default.state.deck.data;
  }

  private isDocEdited(): boolean {
    return !editorStore.default.state.doc || !editorStore.default.state.doc.id || !editorStore.default.state.doc.data;
  }

  private async getDeck(): Promise<Deck | undefined> {
    if (!editorStore.default.state.deck) {
      return undefined;
    }

    const {id: deckId}: Deck = editorStore.default.state.deck;

    const deck: Deck | undefined = await get(`/decks/${deckId}`);

    if (!deck) {
      throw new Error('No deck found in IDB');
    }

    return this.cleanDeck({deck, cleanMeta: false});
  }

  private async getSlides(): Promise<Slide[] | undefined> {
    if (!editorStore.default.state.deck) {
      return undefined;
    }

    const deck: Deck = editorStore.default.state.deck;

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
    if (!editorStore.default.state.doc) {
      return undefined;
    }

    const {id: docId}: Doc = editorStore.default.state.doc;

    const doc: Doc | undefined = await get(`/docs/${docId}`);

    if (!doc) {
      throw new Error('No doc found in IDB');
    }

    return this.cleanDoc({doc, cleanMeta: false});
  }

  private async getParagraphs(): Promise<Paragraph[] | undefined> {
    if (!editorStore.default.state.doc) {
      return undefined;
    }

    const doc: Doc = editorStore.default.state.doc;

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
  }> {
    const zip = new JSZip();

    const content: JSZip = await zip.loadAsync(file);

    const data: FileImportData = await this.parseImportData(content);

    const zippedAssets: {path: string; file: JSZip.JSZipObject}[] = [];

    this.listZipAssets({content, zippedAssets, subPath: '/assets/local/images/'});
    this.listZipAssets({content, zippedAssets, subPath: '/assets/local/data/'});

    // We import the cloud assets only if user is online otherwise it will be possible to display those
    if (!offlineStore.default.state.online) {
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
      ...this.resetImportDataIds(data),
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
    if (!offlineStore.default.state.online) {
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
   * If data are exported from the editor, they contain ids.
   * In case they are imported from other source, such as Figma, these are new data.
   *
   * In any case, we do not want to overwrite existing data but, always create new data, mostly to avoid issue when cloud is used.
   *
   * - This is useful to avoid loosing newer data, assuming user has data sync with the cloud
   * - If user send .ddg files to another user, then data such as "meta" (which we now also delete from the export) would be imported
   * - In addition, with Firebase each deck needs another Id, that's why if a .ddg file is send and imported by another user, it would try to reuse same id which is not possible
   *
   * Finally, we want to reset the owner_id
   */
  private resetImportDataIds({deck, slides, doc, paragraphs}: FileImportData): {data: ImportData} {
    const id: string = uuid();
    const now: Date = new Date();

    const newSlides: Slide[] | undefined = slides?.map((slide: Partial<Slide>) => ({
      data: {
        ...slide.data,
        updated_at: now,
        created_at: now
      },
      id: uuid()
    })) as Slide[] | undefined;

    const newDeck: Deck | undefined = deck
      ? ({
          data: {
            ...deck.data,
            owner_id: authStore.state.authUser?.uid,
            slides: newSlides?.map(({id}: Slide) => id),
            updated_at: now,
            created_at: now
          },
          id
        } as Deck)
      : undefined;

    // Even though per definition paragraphs cannot be null, as long as Stylo and the doc editor are not stable it is worth checking it
    const newParagraphs: Paragraph[] | undefined = paragraphs
      ?.filter((paragraph: Partial<Paragraph>) => paragraph !== null)
      .map((paragraph: Partial<Paragraph>) => ({
        data: {
          ...paragraph.data,
          updated_at: now,
          created_at: now
        },
        id: uuid()
      })) as Paragraph[] | undefined;

    const newDoc: Doc | undefined = doc
      ? ({
          data: {
            ...doc.data,
            owner_id: authStore.state.authUser?.uid,
            paragraphs: newParagraphs?.map(({id}: Paragraph) => id),
            updated_at: now,
            created_at: now
          },
          id
        } as Doc)
      : undefined;

    return {
      data: {
        id,
        ...(newDeck && {deck: this.cleanDeck({deck: newDeck, cleanMeta: true})}),
        ...(newSlides && {slides: newSlides}),
        ...(newDoc && {doc: this.cleanDoc({doc: newDoc, cleanMeta: true})}),
        ...(newParagraphs && {paragraphs: newParagraphs})
      }
    };
  }

  private cleanDeck({deck, cleanMeta}: {deck: Deck; cleanMeta: boolean}): Deck {
    const clone: Deck = {...deck};

    if (cleanMeta) {
      delete clone.data.meta;
    }

    delete clone.data.api_id;
    delete clone.data.deploy;
    delete clone.data.github;

    return clone;
  }

  private cleanDoc({doc, cleanMeta}: {doc: Doc; cleanMeta: boolean}): Doc {
    const clone: Doc = {...doc};

    if (cleanMeta) {
      delete clone.data.meta;
    }

    delete clone.data.deploy;

    return clone;
  }
}
