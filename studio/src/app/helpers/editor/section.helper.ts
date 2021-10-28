import {JSX} from '@stencil/core';

import {Doc, Section} from '@deckdeckgo/editor';
import {getAnchorElement, getSelection, moveCursorToEnd} from '@deckdeckgo/utils';

import errorStore from '../../stores/error.store';
import busyStore from '../../stores/busy.store';
import docStore from '../../stores/doc.store';

import {ParseSectionsUtils} from '../../utils/editor/parse-sections.utils';
import {NodeUtils} from '../../utils/editor/node.utils';

import {getOfflineDoc} from '../../providers/data/docs/doc.offline.provider';
import {getOfflineSection} from '../../providers/data/docs/section.offline.provider';
import {findParagraph} from '../../utils/editor/container.utils';

export class SectionHelper {
  loadDocAndRetrieveSections(docId: string): Promise<JSX.IntrinsicElements[] | null> {
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

        docStore.state.doc = {...doc};

        if (!doc.data.sections || doc.data.sections.length <= 0) {
          resolve([]);
          return;
        }

        const promises: Promise<JSX.IntrinsicElements>[] = doc.data.sections.map((sectionId: string) =>
          this.fetchSection({doc, sectionId})
        );
        const parsedSections: JSX.IntrinsicElements[] = await Promise.all(promises);

        if (!parsedSections || parsedSections.length <= 0) {
          resolve([]);
          return;
        }

        busyStore.state.deckBusy = false;

        resolve(parsedSections);
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve(null);
      }
    });
  }

  private fetchSection({doc, sectionId}: {doc: Doc; sectionId: string}): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      try {
        const section: Section = await getOfflineSection({docId: doc.id, sectionId});

        const element: JSX.IntrinsicElements = await ParseSectionsUtils.parseSection({section});

        resolve(element);
      } catch (err) {
        errorStore.state.error = 'Something went wrong while loading and parsing a section';
        resolve(null);
      }
    });
  }

  initAddSection(docRef: HTMLDeckgoDocElement | undefined): number | undefined {
    if (!docRef) {
      return undefined;
    }

    const selection: Selection | null = getSelection();
    const element: HTMLElement | null = getAnchorElement(selection);

    if (!element) {
      return undefined;
    }

    const paragraph: Node | undefined = findParagraph({element, container: docRef.firstChild});

    if (!paragraph) {
      return undefined;
    }

    const index: number = NodeUtils.nodeIndex(paragraph as HTMLElement) + 1;

    const docObserver: MutationObserver = new MutationObserver(() => {
      docObserver.disconnect();
      moveCursorToEnd(docRef.querySelector(`article *:nth-child(${index + 1})`));
    });

    docObserver.observe(docRef, {childList: true, subtree: true});

    return index;
  }
}
