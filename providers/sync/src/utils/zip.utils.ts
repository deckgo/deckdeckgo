import {Deck, Doc, FileImportData, injectJS, Paragraph, Slide, throwError, UserAsset} from '@deckdeckgo/editor';
import {nanoid} from 'nanoid';
import {AuthStore} from '../stores/auth.store';
import {EnvStore} from '../stores/env.store';
import {EnvironmentCdn} from '../types/env.types';
import {ImportAsset, ImportData} from '../types/import.types';
import {cleanDeck, cleanDoc} from './import.utils';
import {isOnline} from './offline.utils';

export const zip = async ({data, assets}: {data: ImportData; assets: UserAsset[]}): Promise<Blob> => {
  const zip = await initJSZip();

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
};

export const unzip = async (
  file: File
): Promise<{
  data: ImportData;
  assets: ImportAsset[];
}> => {
  const zip = await initJSZip();

  const content: JSZip = await zip.loadAsync(file);

  const data: FileImportData = await parseImportData(content);

  const zippedAssets: {path: string; file: JSZip.JSZipObject}[] = [];

  listZipAssets({content, zippedAssets, subPath: '/assets/local/images/'});
  listZipAssets({content, zippedAssets, subPath: '/assets/local/data/'});

  // We import the cloud assets only if user is online otherwise it will be possible to display those
  if (!isOnline()) {
    listZipAssets({content, zippedAssets, subPath: '/assets/online/images/'});
    listZipAssets({content, zippedAssets, subPath: '/assets/online/data/'});
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
    ...resetImportDataIds(data),
    assets
  };
};

const initJSZip = async (): Promise<JSZip> => {
  const jszip: EnvironmentCdn | undefined = EnvStore.getInstance().get().jszip;

  if (!jszip?.cdn) {
    throwError('No CDN provided to load jszip.');
    return;
  }

  await injectJS({id: 'jszip-script', src: jszip.cdn, module: false});

  return new JSZip();
};

const listZipAssets = ({
  content,
  zippedAssets,
  subPath
}: {
  content: JSZip;
  subPath: string;
  zippedAssets: {path: string; file: JSZip.JSZipObject}[];
}) => {
  content.folder(subPath).forEach((filename: string, file: JSZip.JSZipObject) =>
    zippedAssets.push({
      path: `${subPath}${filename}`,
      file
    })
  );
};

const parseImportData = async (content: JSZip): Promise<FileImportData> => {
  let data: string = await content.file('data.json').async('text');

  // If user is offline, then we load the online content saved in the cloud locally too, better display the content than none
  if (!isOnline()) {
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
};

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
const resetImportDataIds = ({deck, slides, doc, paragraphs}: FileImportData): {data: ImportData} => {
  const id: string = nanoid();
  const now: Date = new Date();

  const newSlides: Slide[] | undefined = slides?.map((slide: Partial<Slide>) => ({
    data: {
      ...slide.data,
      updated_at: now,
      created_at: now
    },
    id: nanoid()
  })) as Slide[] | undefined;

  const newDeck: Deck | undefined = deck
    ? ({
        data: {
          ...deck.data,
          owner_id: AuthStore.getInstance().get()?.uid,
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
      id: nanoid()
    })) as Paragraph[] | undefined;

  const newDoc: Doc | undefined = doc
    ? ({
        data: {
          ...doc.data,
          owner_id: AuthStore.getInstance().get()?.uid,
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
      ...(newDeck && {deck: cleanDeck({deck: newDeck, cleanMeta: true})}),
      ...(newSlides && {slides: newSlides}),
      ...(newDoc && {doc: cleanDoc({doc: newDoc, cleanMeta: true})}),
      ...(newParagraphs && {paragraphs: newParagraphs})
    }
  };
};
