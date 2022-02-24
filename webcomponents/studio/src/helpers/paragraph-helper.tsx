import {Doc, Paragraph} from '@deckdeckgo/editor';
import {getOfflineDoc, getOfflineParagraph} from '@deckdeckgo/offline';
import {JSX} from '@stencil/core';
import editorStore from '../stores/editor.store';
import errorStore from '../stores/error.store';
import readyStore from '../stores/ready.store';
import {ParseParagraphsUtils} from '../utils/parse-paragraphs.utils';

export class ParagraphHelper {
  loadDocAndRetrieveParagraphs(docId: string): Promise<JSX.IntrinsicElements[] | null> {
    return new Promise<JSX.IntrinsicElements[] | null>(async (resolve) => {
      if (!docId) {
        errorStore.state.error = 'Doc is not defined';
        resolve(null);
        return;
      }

      readyStore.state.docReady = false;

      try {
        const doc: Doc = await getOfflineDoc(docId);

        if (!doc || !doc.data) {
          errorStore.state.error = 'No doc could be fetched';
          readyStore.state.docReady = true;
          resolve(null);
          return;
        }

        editorStore.state.doc = {...doc};

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
        errorStore.state.error = err;
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
        errorStore.state.error = 'Something went wrong while loading and parsing a paragraph';
        resolve(undefined);
      }
    });
  }
}
