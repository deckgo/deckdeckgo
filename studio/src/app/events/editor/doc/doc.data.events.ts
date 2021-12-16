import {Doc, DocData, now, Paragraph, ParagraphData, elementIndex, isTextNode, cleanNode, isElementNode} from '@deckdeckgo/editor';

import {debounce} from '@deckdeckgo/utils';

import errorStore from '../../../stores/error.store';
import busyStore from '../../../stores/busy.store';
import editorStore from '../../../stores/editor.store';
import authStore from '../../../stores/auth.store';

import {findParagraph} from '../../../utils/editor/paragraph.utils';

import {createOfflineDoc, updateOfflineDoc} from '../../../providers/data/doc/doc.offline.provider';
import {
  createOfflineParagraph,
  deleteOfflineParagraph,
  updateOfflineParagraph
} from '../../../providers/data/paragraph/paragraph.offline.provider';
import {
  findAddedNodesParagraphs,
  findAddedParagraphs,
  findRemovedNodesParagraphs,
  findUpdatedParagraphs
} from '../../../utils/editor/paragraphs.utils';

export class DocDataEvents {
  private containerRef: HTMLElement;

  private treeObserver: MutationObserver | undefined;
  private attributesObserver: MutationObserver | undefined;
  private dataObserver: MutationObserver | undefined;

  private stackDataMutations: MutationRecord[] = [];

  private readonly debounceUpdateInput: () => void = debounce(async () => await this.updateData(), 500);

  private debounceBusyEnd: () => void = debounce(() => (busyStore.state.busy = false), 500);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.treeObserver = new MutationObserver(this.onTreeMutation);
    this.treeObserver.observe(this.containerRef, {childList: true, subtree: true});

    this.attributesObserver = new MutationObserver(this.onAttributesMutation);
    this.attributesObserver.observe(this.containerRef, {attributes: true, subtree: true});

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true});

    document.addEventListener('markdownDidChange', this.onCustomEventChange);
    document.addEventListener('wordCloudDidChange', this.onCustomEventChange);
    document.addEventListener('codeDidChange', this.onCustomEventChange);
    document.addEventListener('mathDidChange', this.onCustomEventChange);
    document.addEventListener('imgDidChange', this.onCustomEventChange);
  }

  destroy() {
    this.treeObserver?.disconnect();
    this.attributesObserver?.disconnect();
    this.dataObserver?.disconnect();

    document.addEventListener('markdownDidChange', this.onCustomEventChange);
    document.addEventListener('wordCloudDidChange', this.onCustomEventChange);
    document.addEventListener('codeDidChange', this.onCustomEventChange);
    document.addEventListener('mathDidChange', this.onCustomEventChange);
    document.addEventListener('imgDidChange', this.onCustomEventChange);
  }

  private onTreeMutation = async (mutations: MutationRecord[]) => {
    busyStore.state.busy = true;

    await this.addParagraphs(mutations);
    await this.deleteParagraphs(mutations);
    await this.updateAddedNodesParagraphs(mutations);

    this.debounceBusyEnd();
  };

  private onAttributesMutation = async (mutations: MutationRecord[]) => {
    busyStore.state.busy = true;

    await this.updateParagraphs(mutations.filter(({attributeName}: MutationRecord) => ['style'].includes(attributeName)));

    this.debounceBusyEnd();
  };

  private onDataMutation = (mutations: MutationRecord[]) => {
    this.stackDataMutations.push(...mutations);
    this.debounceUpdateInput();
  };

  private onCustomEventChange = async ({detail}: CustomEvent<HTMLElement>) => {
    const paragraph: Node | undefined = findParagraph({element: detail, container: this.containerRef});

    if (!paragraph || isTextNode(paragraph)) {
      return;
    }

    await this.updateParagraph(paragraph as HTMLElement);
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

      const content: string[] = this.toParagraphContent(element);
      if (content && content.length > 0) {
        paragraphData.children = content;
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

        const index: number = elementIndex(paragraphElement);
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

      const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});

      await this.createDoc();

      for (const paragraph of addedParagraphs) {
        await this.createParagraph(paragraph);
      }
    } catch (err) {
      errorStore.state.error = err;
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
      errorStore.state.error = err;
    }
  }

  private async updateAddedNodesParagraphs(mutations: MutationRecord[]) {
    try {
      if (!this.containerRef) {
        return;
      }

      if (!mutations || mutations.length <= 0) {
        return;
      }

      const addedNodesMutations: MutationRecord[] = findAddedNodesParagraphs({mutations, container: this.containerRef});
      const removedNodesMutations: MutationRecord[] = findRemovedNodesParagraphs({mutations, container: this.containerRef});

      await this.updateParagraphs([...addedNodesMutations, ...removedNodesMutations]);
    } catch (err) {
      errorStore.state.error = err;
    }
  }

  private async updateData() {
    if (!this.stackDataMutations || this.stackDataMutations.length <= 0) {
      return;
    }

    busyStore.state.busy = true;

    const mutations: MutationRecord[] = [...this.stackDataMutations];
    this.stackDataMutations = [];

    await this.updateParagraphs(mutations);

    this.debounceBusyEnd();
  }

  private async updateParagraphs(mutations: MutationRecord[]) {
    try {
      if (!this.containerRef) {
        return;
      }

      const updateParagraphs: HTMLElement[] = findUpdatedParagraphs({mutations, container: this.containerRef});

      if (updateParagraphs.length <= 0) {
        return;
      }

      const promises: Promise<void>[] = updateParagraphs.map((paragraph: HTMLElement) => this.updateParagraph(paragraph));
      await Promise.all(promises);
    } catch (err) {
      errorStore.state.error = err;
    }
  }

  private async updateParagraph(paragraph: HTMLElement) {
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

    const paragraphUpdate: Paragraph = {
      id: paragraphId,
      data: {
        nodeName: paragraph.nodeName.toLowerCase()
      }
    };

    // Attributes
    const attrs: Attr[] = Array.from(paragraph.attributes).filter(
      ({nodeName}: Attr) => !['id', 'hydrated', 'class', 'contenteditable', 'paragraph_id'].includes(nodeName)
    );

    paragraphUpdate.data.attributes =
      attrs.length > 0
        ? attrs.reduce(
            (acc: Record<string, string | number | boolean | undefined>, {nodeName, nodeValue}: Attr) => ({
              ...acc,
              ...{[nodeName]: nodeValue}
            }),
            {}
          )
        : null;

    // Content
    const content: string[] = this.toParagraphContent(paragraph);

    if (content && content.length > 0) {
      paragraphUpdate.data.children = content;
    } else {
      paragraphUpdate.data.children = null;
    }

    await updateOfflineParagraph({docId, paragraph: paragraphUpdate});
  }

  private toParagraphContent(paragraph: HTMLElement): string[] {
    return Array.from(paragraph.childNodes).reduce((acc: string[], node: Node) => {
      if (isTextNode(node)) {
        acc.push(node.nodeValue);
      }

      if (isElementNode(node)) {
        const cleanedNode: Node | null = cleanNode({node});

        if (cleanedNode) {
          acc.push((cleanedNode as HTMLElement).outerHTML);
        }
      }

      return acc;
    }, []);
  }
}
