import {Deck, DeckData, Doc, DocData, Meta, Paragraph, Slide} from '@deckdeckgo/editor';
import {nanoid} from 'nanoid';
import {deleteDeck} from '../providers/deck.provider';
import {deleteDoc} from '../providers/doc.provider';
import {getParagraph} from '../providers/paragraph.provider';
import {updateLanding} from '../providers/publish.provider';
import {getSlide} from '../providers/slide.provider';
import {deleteFile} from '../providers/storage.provider';
import {ImportData} from '../types/import.types';
import {importEditorData, importEditorSync} from '../utils/import.utils';

export type DeckOrDoc = {deck: Deck; doc?: never} | {doc: Doc; deck?: never};

export const clone = async (data: DeckOrDoc) => {
  const importData: ImportData = (await cloneDeck(data)) || (await cloneDoc(data));

  await importEditorData(importData);

  // Add new data to list of data to sync
  await importEditorSync(importData);
};

const cloneDeck = async ({deck}: DeckOrDoc): Promise<ImportData | undefined> => {
  if (!deck) {
    return undefined;
  }

  const cloneDeck: Deck = cloneDeckData(deck);

  const promises: Promise<Slide>[] | undefined = deck.data.slides?.map((slideId: string) => getSlide(deck.id, slideId));
  const slides: Slide[] = await Promise.all(promises || []);

  const cloneSlides: Slide[] = slides.map((slide: Slide) => ({
    ...slide,
    id: nanoid()
  }));

  cloneDeck.data.slides = cloneSlides.map(({id}: Slide) => id);

  return {
    id: cloneDeck.id,
    deck: cloneDeck,
    slides: cloneSlides
  };
};

const cloneDoc = async ({doc}: DeckOrDoc): Promise<ImportData | undefined> => {
  if (!doc) {
    return undefined;
  }

  const cloneDoc: Doc = cloneDocData(doc);

  const promises: Promise<Paragraph>[] | undefined = doc.data.paragraphs?.map((paragraphId: string) =>
    getParagraph({docId: doc.id, paragraphId})
  );
  const paragraphs: Paragraph[] = await Promise.all(promises || []);

  const cloneParagraphs: Paragraph[] = paragraphs.map((paragraph: Paragraph) => ({
    ...paragraph,
    id: nanoid()
  }));

  cloneDoc.data.paragraphs = cloneParagraphs.map(({id}: Paragraph) => id);

  return {
    id: cloneDoc.id,
    doc: cloneDoc,
    paragraphs: cloneParagraphs
  };
};

const cloneDeckData = (deck: Deck): Deck => {
  let clone: DeckData = {...deck.data};

  delete clone['slides'];
  delete clone['api_id'];
  delete clone['meta'];
  delete clone['deploy'];
  delete clone['github'];

  const now: Date = new Date();

  return {
    id: nanoid(),
    data: {
      ...clone,
      updated_at: now,
      created_at: now
    }
  };
};

const cloneDocData = (doc: Doc): Doc => {
  let clone: DocData = {...doc.data};

  delete clone['paragraphs'];
  delete clone['meta'];

  const now: Date = new Date();

  return {
    id: nanoid(),
    data: {
      ...clone,
      updated_at: now,
      created_at: now
    }
  };
};

export const loadAndImportDeck = (deck: Deck): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const promises: Promise<Slide>[] | undefined = deck.data.slides?.map((slideId: string) => getSlide(deck.id, slideId));
      const slides: Slide[] = await Promise.all(promises || []);

      await importEditorData({
        id: deck.id,
        deck,
        slides
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const loadAndImportDoc = (doc: Doc): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const promises: Promise<Paragraph | undefined>[] | undefined = doc.data.paragraphs?.map((paragraphId: string) =>
        getParagraph({
          docId: doc.id,
          paragraphId
        })
      );

      const paragraphs: Paragraph[] = (await Promise.all(promises || [])).filter(
        (paragraph: Paragraph | undefined) => paragraph !== undefined
      );

      await importEditorData({
        id: doc.id,
        doc,
        paragraphs
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteDeckOrDoc = async ({data, deleteStorage}: {data: DeckOrDoc; deleteStorage: boolean}) => {
  if (data.doc) {
    const {
      doc: {
        id,
        data: {meta},
        updated_at
      }
    } = data;

    // If a pathname is defined, then there is a storage
    const hasStorage: boolean = hasPathname(meta);

    await Promise.all([deleteStorageFile({meta, folder: 'd', deleteStorage}), deleteDoc({docId: id, updated_at})]);

    // If we deleted the file in the storage, we also want to update the list of posts displayed on the landing page but only if there is a storage
    if (deleteStorage && hasStorage) {
      await updateLanding();
    }

    return;
  }

  const {deck} = data;

  await Promise.all([
    deleteStorageFile({meta: deck.data.meta, folder: 'p', deleteStorage}),
    deleteDeck({deckId: deck.id, updated_at: deck.updated_at})
  ]);
};

const deleteStorageFile = async ({meta, folder, deleteStorage}: {meta: Meta | undefined; folder: 'p' | 'd'; deleteStorage: boolean}) => {
  if (!deleteStorage) {
    // In such mode, published content is not deployed on Firebase
    return;
  }

  if (!hasPathname(meta)) {
    return;
  }

  const {pathname} = meta;
  const filename: string = pathname.replace(`/${folder}/`, '');

  // We do not have the downloadUrl here but, it does not matter much as it is only use to extract the token for secret file which, in this case, is not used because the data is public.
  await deleteFile({
    downloadUrl: undefined,
    fullPath: meta.pathname,
    name: filename
  });
};

const hasPathname = (meta: Meta | undefined): boolean => {
  if (!meta) {
    return false;
  }

  const {pathname} = meta;
  return pathname !== undefined;
};
