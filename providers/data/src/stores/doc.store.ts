import type {Doc} from '@deckdeckgo/editor';
import {Store} from './store';

export class DocStore extends Store<Doc | null> {
  private static instance: DocStore;

  private doc: Doc | null;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!DocStore.instance) {
      DocStore.instance = new DocStore();
    }
    return DocStore.instance;
  }

  set(doc: Doc | null) {
    this.doc = doc;

    this.populate(doc);
  }

  get(): Doc | null {
    return this.doc;
  }
}
