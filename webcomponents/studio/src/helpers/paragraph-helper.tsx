import {Doc, Paragraph, throwError} from '@deckdeckgo/editor';
import {getOfflineParagraph} from '@deckdeckgo/offline';
import {JSX} from '@stencil/core';
import readyStore from '../stores/ready.store';
import {parseParagraph} from '../utils/parse-paragraphs.utils';

export const loadDocAndRetrieveParagraphs = async ({
  docId,
  loadDoc
}: {
  docId: string;
  loadDoc: (docId: string | undefined) => Promise<Doc>;
}): Promise<JSX.IntrinsicElements[] | null> => {
  readyStore.state.docReady = false;

  try {
    const doc: Doc = await loadDoc(docId);

    if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
      readyStore.state.docReady = true;
      return [];
    }

    const promises: Promise<JSX.IntrinsicElements | undefined>[] = doc.data.paragraphs.map((paragraphId: string) =>
      fetchParagraph({doc, paragraphId})
    );
    const parsedParagraphs: (JSX.IntrinsicElements | undefined)[] = await Promise.all(promises);

    const paragraphs: JSX.IntrinsicElements[] = parsedParagraphs.filter(
      (paragraph: JSX.IntrinsicElements | undefined) => paragraph !== undefined
    );

    if (!paragraphs || paragraphs.length <= 0) {
      readyStore.state.docReady = true;
      return [];
    }

    readyStore.state.docReady = true;
    return paragraphs;
  } catch (err) {
    throwError(err);
    return null;
  }
};

const fetchParagraph = async ({doc, paragraphId}: {doc: Doc; paragraphId: string}): Promise<JSX.IntrinsicElements | undefined> => {
  try {
    const paragraph: Paragraph | undefined = await getOfflineParagraph({docId: doc.id, paragraphId});

    if (!paragraph) {
      return undefined;
    }

    const element: JSX.IntrinsicElements = parseParagraph({paragraph});

    return element;
  } catch (err) {
    throwError('Something went wrong while loading and parsing a paragraph');
    return undefined;
  }
};
