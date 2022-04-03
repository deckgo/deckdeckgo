import {Deck, Doc, Paragraph, Slide, UserAsset} from '@deckdeckgo/editor';
import {get, getMany} from 'idb-keyval';
import {DeckStore} from '../stores/deck.store';
import {DocStore} from '../stores/doc.store';
import {
  getDeckBackgroundImage,
  getParagraphsLocalImages,
  getParagraphsOnlineImages,
  getSlidesLocalCharts,
  getSlidesLocalImages,
  getSlidesOnlineCharts,
  getSlidesOnlineImages
} from '../utils/assets.utils';
import {save} from '../utils/file-system.utils';
import {cleanDeck, cleanDoc, importEditorAssets, importEditorData, importEditorSync} from '../utils/import.utils';
import {unzip, zip} from '../utils/zip.utils';

export const importData = async (file: File): Promise<'doc' | 'deck'> => {
  const {data, assets} = await unzip(file);

  await importEditorAssets(assets);
  await importEditorData(data);

  const {doc} = data;

  const result: 'doc' | 'deck' = doc !== undefined ? 'doc' : 'deck';

  await importEditorSync(data);

  return result;
};

export const exportData = async ({types}: {types: FilePickerAcceptType[]}) => {
  if (!isDeckEdited() && !isDocEdited()) {
    throw new Error('No deck or doc found');
  }

  const deck: Deck | undefined = await getDeck();
  const slides: Slide[] | undefined = await getSlides();

  const doc: Doc | undefined = await getDoc();
  const paragraphs: Paragraph[] | undefined = await getParagraphs();

  const localSlidesImages: UserAsset[] = await getSlidesLocalImages({deck: DeckStore.getInstance().get()});
  const onlineSlidesImages: UserAsset[] = await getSlidesOnlineImages({deck: DeckStore.getInstance().get()});

  const localSlidesCharts: UserAsset[] = await getSlidesLocalCharts({deck: DeckStore.getInstance().get()});
  const onlineSlidesCharts: UserAsset[] = await getSlidesOnlineCharts({deck: DeckStore.getInstance().get()});

  const deckBackground: UserAsset | undefined = await getDeckBackgroundImage();

  const localParagraphsImages: UserAsset[] = await getParagraphsLocalImages({doc: DocStore.getInstance().get()});
  const onlineParagraphsImages: UserAsset[] = await getParagraphsOnlineImages({doc: DocStore.getInstance().get()});

  const blob: Blob = await zip({
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

  await save({
    filename: DeckStore.getInstance().get()?.data?.name || DocStore.getInstance().get()?.data?.name,
    blob,
    types
  });
};

const isDeckEdited = (): boolean => {
  return !DeckStore.getInstance().get() || !DeckStore.getInstance().get().id || !DeckStore.getInstance().get().data;
};

const isDocEdited = (): boolean => {
  return !DocStore.getInstance().get() || !DocStore.getInstance().get().id || !DocStore.getInstance().get().data;
};

const getDeck = async (): Promise<Deck | undefined> => {
  if (!DeckStore.getInstance().get()) {
    return undefined;
  }

  const {id: deckId}: Deck = DeckStore.getInstance().get();

  const deck: Deck | undefined = await get(`/decks/${deckId}`);

  if (!deck) {
    throw new Error('No deck found in IDB');
  }

  return cleanDeck({deck, cleanMeta: false});
};

const getSlides = async (): Promise<Slide[] | undefined> => {
  if (!DeckStore.getInstance().get()) {
    return undefined;
  }

  const deck: Deck = DeckStore.getInstance().get();

  if (!deck.data.slides || deck.data.slides.length <= 0) {
    return [];
  }

  try {
    const keys: string[] = deck.data.slides.map((slideId: string) => `/decks/${deck.id}/slides/${slideId}`);
    return getMany<Slide>(keys);
  } catch (err) {
    throw new Error('Error while fetching slides');
  }
};

const getDoc = async (): Promise<Doc | undefined> => {
  if (!DocStore.getInstance().get()) {
    return undefined;
  }

  const {id: docId}: Doc = DocStore.getInstance().get();

  const doc: Doc | undefined = await get(`/docs/${docId}`);

  if (!doc) {
    throw new Error('No doc found in IDB');
  }

  return cleanDoc({doc, cleanMeta: false});
};

const getParagraphs = async (): Promise<Paragraph[] | undefined> => {
  if (!DocStore.getInstance().get()) {
    return undefined;
  }

  const doc: Doc = DocStore.getInstance().get();

  if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
    return [];
  }

  try {
    const keys: string[] = doc.data.paragraphs.map((paragraphId: string) => `/docs/${doc.id}/paragraphs/${paragraphId}`);
    return getMany<Paragraph>(keys);
  } catch (err) {
    throw new Error('Error while fetching paragraphs');
  }
};
