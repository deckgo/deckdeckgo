import {Doc, Paragraph, throwError} from '@deckdeckgo/editor';
import {getOfflineParagraph} from '@deckdeckgo/offline';
import {JSX} from '@stencil/core';
import readyStore from '../stores/ready.store';
import {ParseParagraphsUtils} from '../utils/parse-paragraphs.utils';

export class ParagraphHelper {
  loadDocAndRetrieveParagraphs({
    docId,
    loadDoc
  }: {
    docId: string;
    loadDoc: (docId: string | undefined) => Promise<Doc>;
  }): Promise<JSX.IntrinsicElements[] | null> {
    return new Promise<JSX.IntrinsicElements[] | null>(async (resolve) => {
      readyStore.state.docReady = false;

      try {
        const doc: Doc = await loadDoc(docId);

        if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
          readyStore.state.docReady = true;
          resolve([]);
          return;
        }

        const promises: Promise<JSX.IntrinsicElements | undefined>[] = doc.data.paragraphs.map((paragraphId: string) =>
          this.fetchParagraph({doc, paragraphId})
        );
        const parsedParagraphs: (JSX.IntrinsicElements | undefined)[] = await Promise.all(promises);

        const paragraphs: JSX.IntrinsicElements[] = parsedParagraphs.filter(
          (paragraph: JSX.IntrinsicElements | undefined) => paragraph !== undefined
        );

        if (!paragraphs || paragraphs.length <= 0) {
          readyStore.state.docReady = true;
          resolve([]);
          return;
        }

        readyStore.state.docReady = true;
        resolve(paragraphs);
      } catch (err) {
        throwError(err);
        resolve(null);
      }
    });
  }

  private fetchParagraph({doc, paragraphId}: {doc: Doc; paragraphId: string}): Promise<JSX.IntrinsicElements | undefined> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      try {
        const paragraph: Paragraph | undefined = await getOfflineParagraph({docId: doc.id, paragraphId});

        if (!paragraph) {
          resolve(undefined);
          return;
        }

        const element: JSX.IntrinsicElements = await ParseParagraphsUtils.parseParagraph({paragraph});

        resolve(element);
      } catch (err) {
        throwError('Something went wrong while loading and parsing a paragraph');
        resolve(undefined);
      }
    });
  }
}
