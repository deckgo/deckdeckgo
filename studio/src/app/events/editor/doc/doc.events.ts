import {Doc, DocData, now, Paragraph, ParagraphData} from '@deckdeckgo/editor';
import {cleanContent} from '@deckdeckgo/deck-utils';

import errorStore from '../../../stores/error.store';
import busyStore from '../../../stores/busy.store';
import editorStore from '../../../stores/editor.store';
import authStore from '../../../stores/auth.store';

import {findParagraph, isParagraph} from '../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';

import {createOfflineDoc, updateOfflineDoc} from '../../../providers/data/docs/doc.offline.provider';
import {
  createOfflineParagraph,
  deleteOfflineParagraph,
  updateOfflineParagraph
} from '../../../providers/data/docs/paragraph.offline.provider';
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
    await this.addParagraphs(mutations);
    await this.deleteParagraphs(mutations);
  };

  private onAttributesMutation = async (mutations: MutationRecord[]) => {
    await this.updateParagraphs(mutations);
  };

  private onDataMutation = (mutations: MutationRecord[]) => {
    this.stackDataMutations.push(...mutations);
    this.debounceUpdateInput();
  };

  private createDoc(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (editorStore.state.doc) {
          resolve();
          return;
        }

        let docData: DocData = {
          name: `Document ${now()}`,
          owner_id: authStore.state.authUser?.uid
        };

        const persistedDoc: Doc = await createOfflineDoc(docData);
        editorStore.state.doc = {...persistedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async createParagraph(element: HTMLElement) {
    const {id: paragraphId}: Paragraph = await this.postParagraph(element);
    await this.updateDocParagraphList({paragraphId, paragraphElement: element});
  }

  private async deleteParagraph(element: Node): Promise<string | undefined> {
    if (element.nodeType === Node.TEXT_NODE || element.nodeType === Node.COMMENT_NODE) {
      return;
    }

    const paragraphId: string = (element as HTMLElement).getAttribute('paragraph_id');

    if (!paragraphId) {
      return undefined;
    }

    await deleteOfflineParagraph({docId: editorStore.state.doc.id, paragraphId: paragraphId});

    return paragraphId;
  }

  private postParagraph(element: HTMLElement): Promise<Paragraph> {
    return new Promise<Paragraph>(async (resolve) => {
      const paragraphData: ParagraphData = {
        nodeName: element.nodeName.toLowerCase()
      };

      const content: string = await cleanContent(element.innerHTML);
      if (content && content.length > 0) {
        paragraphData.content = content;
      }

      const persistedParagraph: Paragraph = await createOfflineParagraph({docId: editorStore.state.doc.id, paragraphData: paragraphData});

      if (persistedParagraph && persistedParagraph.id) {
        element.setAttribute('paragraph_id', persistedParagraph.id);
      }

      resolve(persistedParagraph);
    });
  }

  private updateDocParagraphList({paragraphId, paragraphElement}: {paragraphId: string; paragraphElement: HTMLElement}): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const doc: Doc = {...editorStore.state.doc};

        if (!doc && !doc.data) {
          reject('Missing doc to add the paragraph to the list');
          return;
        }

        if (!paragraphId) {
          reject('Missing paragraph ID to create or update the doc');
          return;
        }

        if (!doc.data.paragraphs || doc.data.paragraphs.length <= 0) {
          doc.data.paragraphs = [];
        }

        const index: number = NodeUtils.nodeIndex(paragraphElement);
        doc.data.paragraphs = [...doc.data.paragraphs.slice(0, index), paragraphId, ...doc.data.paragraphs.slice(index)];

        const updatedDoc: Doc = await updateOfflineDoc(doc);
        editorStore.state.doc = {...updatedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private filterDocParagraphList(paragraphIds: (string | undefined)[]): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const doc: Doc = {...editorStore.state.doc};

        if (!doc && !doc.data) {
          reject('Missing doc to update the paragraph to the list');
          return;
        }

        if (doc.data.paragraphs.length <= 0) {
          resolve();
          return;
        }

        const filterParagraphIds: string[] = paragraphIds.filter((id: string | undefined) => id !== undefined);

        if (!filterParagraphIds) {
          resolve();
          return;
        }

        doc.data.paragraphs = [...doc.data.paragraphs.filter((paragraphId: string) => !filterParagraphIds.includes(paragraphId))];

        const updatedDoc: Doc = await updateOfflineDoc(doc);
        editorStore.state.doc = {...updatedDoc};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async addParagraphs(mutations: MutationRecord[]) {
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
        await this.createParagraph(paragraph);
      }
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  private async deleteParagraphs(mutations: MutationRecord[]) {
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

      const promises: Promise<string | undefined>[] = removedNodes.map((node: Node) => this.deleteParagraph(node));
      const removedParagraphIds: (string | undefined)[] = await Promise.all(promises);

      await this.filterDocParagraphList(removedParagraphIds);
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

    await this.updateParagraphs(mutations);
  }

  private async updateParagraphs(mutations: MutationRecord[]) {
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

      const promises: Promise<void>[] = updateParagraphs.map((paragraph: HTMLElement) => this.updateParagraph(paragraph));
      await Promise.all(promises);
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  async updateParagraph(paragraph: HTMLElement) {
    if (!paragraph || !paragraph.nodeName) {
      return;
    }

    const docId: string = editorStore.state.doc.id;

    if (!docId || docId === undefined || docId === '') {
      errorStore.state.error = 'Doc is not defined';
      return;
    }

    const paragraphId: string = paragraph.getAttribute('paragraph_id');

    if (!paragraphId) {
      errorStore.state.error = 'Paragraph is not defined';
      return;
    }

    // TODO: we loose the created_at information here trade of not getting the paragraph from indexedDB for performance reason

    const paragraphUpdate: Paragraph = {
      id: paragraphId,
      data: {
        nodeName: paragraph.nodeName.toLowerCase()
      }
    };

    const content: string = await cleanContent(paragraph.innerHTML);
    if (content && content.length > 0) {
      paragraphUpdate.data.content = content;
    } else {
      paragraphUpdate.data.content = null;
    }

    await updateOfflineParagraph({docId, paragraph: paragraphUpdate});
  }
}
