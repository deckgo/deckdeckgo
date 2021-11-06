import {CreateDoc, Doc, DocData, DocEntries, DeleteDoc, GetDoc, UpdateDoc, SnapshotDoc} from '@deckdeckgo/editor';

import {createEntry, deleteEntry, entries, getEntry, snapshotEntry, updateEntry} from '../../utils/data/firestore.queries';

export const docEntries: DocEntries = (userId: string): Promise<Doc[]> => {
  return entries<DocData>({userId, collection: 'doc'});
};

export const deleteDoc: DeleteDoc = (docId: string): Promise<void> => {
  return deleteEntry({id: docId, collection: 'doc'});
};

export const createDoc: CreateDoc = (doc: DocData): Promise<Doc> => {
  return createEntry<DocData>({data: doc, collection: 'doc'});
};

export const getDoc: GetDoc = (docId: string): Promise<Doc> => {
  return getEntry<DocData>({id: docId, collection: 'doc'});
};

export const updateDoc: UpdateDoc = (doc: Doc): Promise<Doc> => {
  return updateEntry<Doc>({entry: doc, collection: 'doc'});
};

export const snapshotDoc: SnapshotDoc = async ({
  docId,
  onNext,
  onError
}: {
  docId: string;
  onNext: (snapshot: Doc) => void;
  onError?: (error: string) => void;
}): Promise<() => void | undefined> => {
  return snapshotEntry<DocData>({id: docId, collection: 'doc', onNext, onError});
};
