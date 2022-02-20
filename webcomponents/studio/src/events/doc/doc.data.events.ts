import {cleanNode, Doc, DocData, elementIndex, isElementNode, isTextNode, now, Paragraph, ParagraphData} from '@deckdeckgo/editor';
import {nanoid} from 'nanoid';
import {editorConfig} from '../../config/editor';
import {createOfflineDoc, updateOfflineDoc} from '../../providers/doc.offline.provider';
import {createOfflineParagraph, deleteOfflineParagraph, updateOfflineParagraph} from '../../providers/paragraph.offline.provider';
import authStore from '../../stores/auth.store';
import busyStore from '../../stores/busy.store';
import editorStore from '../../stores/editor.store';
import errorStore from '../../stores/error.store';

export class DocDataEvents {
  init() {
    document.addEventListener('addParagraphs', this.onAddParagraphs);
    document.addEventListener('deleteParagraphs', this.onDeleteParagraphs);
    document.addEventListener('updateParagraphs', this.onUpdateParagraphs);
  }

  destroy() {
    document.removeEventListener('addParagraphs', this.onAddParagraphs);
    document.removeEventListener('deleteParagraphs', this.onDeleteParagraphs);
    document.removeEventListener('updateParagraphs', this.onUpdateParagraphs);
  }

  private onAddParagraphs = async ({detail: addedParagraphs}: CustomEvent<HTMLElement[]>) => {
    try {
      busyStore.state.busy = true;

      await this.createDoc();

      for (const paragraph of addedParagraphs) {
        await this.createParagraph(paragraph);
      }
    } catch (err) {
      errorStore.state.error = err;
    }

    busyStore.state.busy = false;
  };

  private onDeleteParagraphs = async ({detail: removedParagraphs}: CustomEvent<HTMLElement[]>) => {
    try {
      busyStore.state.busy = true;

      const promises: Promise<string | undefined>[] = removedParagraphs.map((paragraph: HTMLElement) => this.deleteParagraph(paragraph));
      const removedParagraphIds: (string | undefined)[] = await Promise.all(promises);

      await this.filterDocParagraphList(removedParagraphIds);
    } catch (err) {
      errorStore.state.error = err;
    }

    busyStore.state.busy = false;
  };

  private onUpdateParagraphs = async ({detail: updatedParagraphs}: CustomEvent<HTMLElement[]>) => {
    try {
      busyStore.state.busy = true;

      // In case of copy-paste, the browser might proceed with a delete-update for which we do not get paragraph_id.
      // It might copy a <p/> within a <div/> instead of creating a child of the container for the new <p/>
      const promises: Promise<void>[] = updatedParagraphs
        .filter((paragraph: HTMLElement) => paragraph.hasAttribute('paragraph_id'))
        .map((paragraph: HTMLElement) => this.updateParagraph(paragraph));
      await Promise.all(promises);
    } catch (err) {
      errorStore.state.error = err;
    }

    busyStore.state.busy = false;
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

  private async deleteParagraph(element: HTMLElement): Promise<string | undefined> {
    const paragraphId: string = element.getAttribute('paragraph_id');

    // Cannot happen because paragraph_id are filter in stylo before triggering the delete paragraphs event
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

      const attributes: Record<string, string | number | boolean | undefined> | null = this.paragraphAttributes(element);
      if (attributes) {
        paragraphData.attributes = attributes;
      }

      const content: string[] = this.paragraphContent(element);
      if (content && content.length > 0) {
        paragraphData.children = content;
      }

      const paragraphId: string = nanoid();

      const persistedParagraph: Paragraph = await createOfflineParagraph({
        docId: editorStore.state.doc.id,
        paragraphData: paragraphData,
        paragraphId
      });

      element.setAttribute('paragraph_id', paragraphId);

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

  private async updateParagraph(paragraph: HTMLElement) {
    if (!paragraph || !paragraph.nodeName) {
      return;
    }

    const docId: string = editorStore.state.doc.id;

    if (!docId || docId === undefined || docId === '') {
      errorStore.state.error = 'Doc is not defined';
      return;
    }

    const paragraphId: string | null = paragraph.getAttribute('paragraph_id');

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
    paragraphUpdate.data.attributes = this.paragraphAttributes(paragraph);

    // Content
    const content: string[] = this.paragraphContent(paragraph);

    if (content && content.length > 0) {
      paragraphUpdate.data.children = content;
    } else {
      paragraphUpdate.data.children = null;
    }

    await updateOfflineParagraph({docId, paragraph: paragraphUpdate});
  }

  private paragraphContent(paragraph: HTMLElement): string[] {
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

  private paragraphAttributes(paragraph: HTMLElement): Record<string, string | number | boolean | undefined> | null {
    const attrs: Attr[] = Array.from(paragraph.attributes).filter(
      ({nodeName}: Attr) =>
        !['placeholder', 'data-gramm', 'class', 'spellcheck', 'contenteditable', ...editorConfig.excludeAttributes].includes(nodeName)
    );

    return attrs.length > 0
      ? attrs.reduce(
          (acc: Record<string, string | number | boolean | undefined>, {nodeName, nodeValue}: Attr) => ({
            ...acc,
            ...{[nodeName]: nodeValue}
          }),
          {}
        )
      : null;
  }
}
