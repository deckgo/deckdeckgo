import {JSX} from '@stencil/core';

import {Doc, Paragraph} from '@deckdeckgo/editor';

import errorStore from '../../stores/error.store';
import busyStore from '../../stores/busy.store';
import editorStore from '../../stores/editor.store';

import {ParseParagraphsUtils} from '../../utils/editor/parse-paragraphs.utils';

import {getOfflineDoc} from '../../providers/data/docs/doc.offline.provider';
import {getOfflineParagraph} from '../../providers/data/docs/paragraph.offline.provider';

export class ParagraphHelper {
  loadDocAndRetrieveParagraphs(docId: string): Promise<JSX.IntrinsicElements[] | null> {
    return new Promise<JSX.IntrinsicElements[] | null>(async (resolve) => {
      if (!docId) {
        errorStore.state.error = 'Doc is not defined';
        resolve(null);
        return;
      }

      busyStore.state.deckBusy = true;

      try {
        const doc: Doc = await getOfflineDoc(docId);

        if (!doc || !doc.data) {
          errorStore.state.error = 'No doc could be fetched';
          resolve(null);
          return;
        }

        editorStore.state.doc = {...doc};

        if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
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
          resolve([]);
          return;
        }

        busyStore.state.deckBusy = false;

        resolve(paragraphs);
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
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
