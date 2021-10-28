import {Doc, DocData, now, Section, SectionData} from '@deckdeckgo/editor';
import {cleanContent} from '@deckdeckgo/deck-utils';

import errorStore from '../../../stores/error.store';
import busyStore from '../../../stores/busy.store';
import docStore from '../../../stores/doc.store';
import authStore from '../../../stores/auth.store';

import {findParagraph, isParagraph} from '../../../utils/editor/container.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';

import {createOfflineDoc, updateOfflineDoc} from '../../../providers/data/docs/doc.offline.provider';
import {createOfflineSection, deleteOfflineSection, updateOfflineSection} from '../../../providers/data/docs/section.offline.provider';
import {debounce} from '@deckdeckgo/utils';

export class DocEvents {
  private containerRef: HTMLElement;

  private treeObserver: MutationObserver | undefined;
  private attributesObserver: MutationObserver | undefined;
  private dataObserver: MutationObserver | undefined;

  private stackDataMutations: MutationRecord[] = [];

  private readonly debounceUpdateInput: () => void = debounce(async () => await this.updateData(), 500);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.treeObserver = new MutationObserver(this.onTreeMutation);
    this.treeObserver.observe(this.containerRef, {childList: true, subtree: true});

    this.attributesObserver = new MutationObserver(this.onAttributesMutation);
    this.attributesObserver.observe(this.containerRef, {attributes: true, subtree: true});

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true});
  }

  destroy() {
    this.treeObserver?.disconnect();
    this.attributesObserver?.disconnect();
    this.dataObserver?.disconnect();
  }

  private onTreeMutation = async (mutations: MutationRecord[]) => {
    await this.addSections(mutations);
    await this.deleteSections(mutations);
  };

  private onAttributesMutation = async (mutations: MutationRecord[]) => {
    await this.updateSections(mutations);
  };

  private onDataMutation = (mutations: MutationRecord[]) => {
    this.stackDataMutations.push(...mutations);
    this.debounceUpdateInput();
  };

  private createDoc(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (docStore.state.doc) {
          resolve();
          return;
        }

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

  private async deleteSection(element: Node): Promise<string | undefined> {
    if (element.nodeType === Node.TEXT_NODE || element.nodeType === Node.COMMENT_NODE) {
      return;
    }

    const sectionId: string = (element as HTMLElement).getAttribute('section_id');

    if (!sectionId) {
      return undefined;
    }

    await deleteOfflineSection({docId: docStore.state.doc.id, sectionId});

    return sectionId;
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
          reject('Missing doc to add the section to the list');
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

  private filterDocSectionList(sectionIds: (string | undefined)[]): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const doc: Doc = {...docStore.state.doc};

        if (!doc && !doc.data) {
          reject('Missing doc to update the section to the list');
          return;
        }

        if (doc.data.sections.length <= 0) {
          resolve();
          return;
        }

        const filterSectionIds: string[] = sectionIds.filter((id: string | undefined) => id !== undefined);

        if (!filterSectionIds) {
          resolve();
          return;
        }

        doc.data.sections = [...doc.data.sections.filter((sectionId: string) => !filterSectionIds.includes(sectionId))];

        const updatedDoc: Doc = await updateOfflineDoc(doc);
        docStore.state.doc = {...updatedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async addSections(mutations: MutationRecord[]) {
    try {
      if (!this.containerRef) {
        return;
      }

      if (!mutations || mutations.length <= 0) {
        return;
      }

      const addedNodes: Node[] = mutations.reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

      const addedParagraphs: HTMLElement[] = this.filterParagraphs(addedNodes);

      await this.createDoc();

      for (const paragraph of addedParagraphs) {
        await this.createSection(paragraph);
      }
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  private async deleteSections(mutations: MutationRecord[]) {
    try {
      if (!this.containerRef) {
        return;
      }

      if (!mutations || mutations.length <= 0) {
        return;
      }

      const removedNodes: Node[] = mutations.reduce(
        (acc: Node[], {removedNodes}: MutationRecord) => [...acc, ...Array.from(removedNodes)],
        []
      );

      const promises: Promise<string | undefined>[] = removedNodes.map((node: Node) => this.deleteSection(node));
      const removedSectionIds: (string | undefined)[] = await Promise.all(promises);

      await this.filterDocSectionList(removedSectionIds);
    } catch (err) {
      console.log(err);

      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  private filterParagraphs(nodes: Node[]): HTMLElement[] {
    return nodes
      .filter((node: Node) => isParagraph({element: node, container: this.containerRef}))
      .filter(
        (paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE
      ) as HTMLElement[];
  }

  private async updateData() {
    if (!this.stackDataMutations || this.stackDataMutations.length <= 0) {
      return;
    }

    const mutations: MutationRecord[] = [...this.stackDataMutations];
    this.stackDataMutations = [];

    await this.updateSections(mutations);
  }

  private async updateSections(mutations: MutationRecord[]) {
    try {
      if (!this.containerRef) {
        return;
      }

      if (!mutations || mutations.length <= 0) {
        return;
      }

      const nodes: Node[] = mutations.reduce((acc: Node[], {target}: MutationRecord) => [...acc, target], []);

      const updateParagraphs: HTMLElement[] = [
        ...new Set(
          nodes
            .map((node: Node) => findParagraph({element: node, container: this.containerRef}))
            .filter(
              (paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE
            ) as HTMLElement[]
        )
      ];

      const promises: Promise<void>[] = updateParagraphs.map((paragraph: HTMLElement) => this.updateSection(paragraph));
      await Promise.all(promises);
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  async updateSection(section: HTMLElement) {
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

    // TODO: we loose the created_at information here trade of not getting the section from indexedDB for performance reason

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
