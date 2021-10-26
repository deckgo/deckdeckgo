import {debounce} from '@deckdeckgo/utils';

import {Doc, DocData, now, Section, SectionData} from '@deckdeckgo/editor';
import {cleanContent} from '@deckdeckgo/deck-utils';

import errorStore from '../../../stores/error.store';
import busyStore from '../../../stores/busy.store';
import docStore from '../../../stores/doc.store';
import authStore from '../../../stores/auth.store';

import {findParagraph} from '../../../utils/editor/container.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';

import {createOfflineDoc, updateOfflineDoc} from '../../../providers/data/docs/doc.offline.provider';
import {createOfflineSection, updateOfflineSection} from '../../../providers/data/docs/section.offline.provider';

export class DocEvents {
  private docRef: HTMLDeckgoDocElement;

  private observer: MutationObserver | undefined;

  private readonly debounceUpdateSections: () => void = debounce(async () => await this.updateSections(), 500);

  private mutations: MutationRecord[] = [];

  init(docRef: HTMLDeckgoDocElement) {
    this.docRef = docRef;

    this.observer = new MutationObserver(this.onMutation);
    this.observer.observe(this.docRef, {childList: true, subtree: true, characterData: true, attributes: true});
  }

  observeCreateDoc() {
    const docObserver: MutationObserver = new MutationObserver(this.onCreateDoc);
    docObserver.observe(this.docRef, {childList: true, subtree: true, characterData: true, attributes: true});
  }

  destroy() {
    this.observer?.disconnect();
  }

  private onMutation = (mutations: MutationRecord[]) => {
    this.mutations.push(...mutations);

    this.debounceUpdateSections();
  };

  private onCreateDoc = async (mutations: MutationRecord[], observer: MutationObserver) => {
    try {
      observer.disconnect();

      if (!this.docRef) {
        return;
      }

      const elements: Node[] = [...new Set(mutations.map(({addedNodes}: MutationRecord) => addedNodes[0]))];

      busyStore.state.deckBusy = true;

      await this.createNewDoc();

      for (const element of Array.from(elements)) {
        await this.createSection(element as HTMLElement);
      }
    } catch (err) {
      errorStore.state.error = err;
    }

    busyStore.state.deckBusy = false;
  };

  private createNewDoc(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        let docData: DocData = {
          name: `Document ${now()}`,
          owner_id: authStore.state.authUser?.uid
        };

        const persistedDoc: Doc = await createOfflineDoc(docData);
        docStore.state.doc = {...persistedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async createSection(element: HTMLElement) {
    const {id: sectionId}: Section = await this.postSection(element);
    await this.updateDocSectionList({sectionId, sectionElement: element});
  }

  private postSection(element: HTMLElement): Promise<Section> {
    return new Promise<Section>(async (resolve) => {
      const sectionData: SectionData = {
        nodeName: element.nodeName.toLowerCase()
      };

      const content: string = await cleanContent(element.innerHTML);
      if (content && content.length > 0) {
        sectionData.content = content;
      }

      const persistedSection: Section = await createOfflineSection({docId: docStore.state.doc.id, sectionData});

      if (persistedSection && persistedSection.id) {
        element.setAttribute('section_id', persistedSection.id);
      }

      resolve(persistedSection);
    });
  }

  private updateDocSectionList({sectionId, sectionElement}: {sectionId: string; sectionElement: HTMLElement}): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const doc: Doc = {...docStore.state.doc};

        if (!doc && !doc.data) {
          reject('Missing doc to add the slide to the list');
          return;
        }

        if (!sectionId) {
          reject('Missing section ID to create or update the doc');
          return;
        }

        if (!doc.data.sections || doc.data.sections.length <= 0) {
          doc.data.sections = [];
        }

        const index: number = NodeUtils.nodeIndex(sectionElement);
        doc.data.sections = [...doc.data.sections.slice(0, index), sectionId, ...doc.data.sections.slice(index)];

        const updatedDoc: Doc = await updateOfflineDoc(doc);
        docStore.state.doc = {...updatedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async updateSections() {
    try {
      if (!this.docRef) {
        return;
      }

      if (!this.mutations || this.mutations.length <= 0) {
        return;
      }

      // TODO: contentEditable duplicate section_id on enter FIXME
      // We might not need the stack
      const mutations: MutationRecord[] = [...this.mutations];
      this.mutations = [];

      const nodes: Node[] = [...new Set(mutations.map(({target}: MutationRecord) => target))];

      const sections: Node[] = nodes
        .map((node: Node) => findParagraph({element: node, container: this.docRef.firstChild}))
        .filter((filteredParagraph: Node | undefined) => filteredParagraph !== undefined);

      for (const element of sections) {
        const section: HTMLElement = element as HTMLElement;

        if (!section.getAttribute('section_id')) {
          await this.createSection(section);
        } else {
          await this.updateSection(section);
        }
      }
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  private async updateSection(section: HTMLElement) {
    if (!section || !section.nodeName) {
      return;
    }

    const docId: string = docStore.state.doc.id;

    if (!docId || docId === undefined || docId === '') {
      errorStore.state.error = 'Doc is not defined';
      return;
    }

    const sectionId: string = section.getAttribute('section_id');

    if (!sectionId) {
      errorStore.state.error = 'Section is not defined';
      return;
    }

    // TODO: we loose the created_at information here trade of not getting thee slide from indexedDB for performance reason

    const sectionUpdate: Section = {
      id: sectionId,
      data: {
        nodeName: section.nodeName.toLowerCase()
      }
    };

    const content: string = await cleanContent(section.innerHTML);
    if (content && content.length > 0) {
      sectionUpdate.data.content = content;
    } else {
      sectionUpdate.data.content = null;
    }

    await updateOfflineSection({docId, section: sectionUpdate});
  }
}
