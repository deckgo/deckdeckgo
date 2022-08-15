import {Doc, DocData} from '../models/data/doc';

export interface DocEntries {
  (userId: string): Promise<Doc[]>;
}

export interface DeleteDoc {
  (deckId: string, updated_at?: Date | number | BigInt): Promise<void>;
}

export interface CreateDoc {
  (deck: DocData): Promise<Doc>;
}

export interface GetDoc {
  (deckId: string): Promise<Doc>;
}

export interface UpdateDoc {
  (deck: Doc): Promise<Doc>;
}

export interface SnapshotDoc {
  ({docId, onNext, onError}: {docId: string; onNext: (snapshot: Doc) => Promise<void>; onError?: (error: string) => void}): Promise<
    () => void | undefined
  >;
}
