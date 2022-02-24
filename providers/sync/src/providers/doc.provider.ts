import {DeleteDoc, Doc, DocEntries, SnapshotDoc} from '@deckdeckgo/editor';
import {DocStore} from '../stores/doc.store';
import {EnvStore} from '../stores/env.store';
import {cloudProvider} from '../utils/providers.utils';

export const docs = async (userId: string): Promise<Doc[]> => {
  if (EnvStore.getInstance().cloud()) {
    const {docEntries}: {docEntries: DocEntries} = await cloudProvider<{docEntries: DocEntries}>();

    return docEntries(userId);
  }

  throw new Error('Not implemented');
};

export const deleteDoc = async (docId: string): Promise<void> => {
  if (EnvStore.getInstance().cloud()) {
    const {deleteDoc: deleteUserDoc}: {deleteDoc: DeleteDoc} = await cloudProvider<{deleteDoc: DeleteDoc}>();

    return deleteUserDoc(docId);
  }

  throw new Error('Not implemented');
};

export const snapshotDoc = (): Promise<() => void | undefined> =>
  snapshotUserDoc({
    docId: DocStore.getInstance().get().id,
    onNext: (snapshot: Doc) => DocStore.getInstance().set({...snapshot})
  });

export const snapshotUserDoc = async ({
  docId,
  onNext
}: {
  docId: string;
  onNext: (snapshot: Doc) => void;
}): Promise<() => void | undefined> => {
  if (EnvStore.getInstance().cloud()) {
    const {snapshotDoc: snapshotUserDoc}: {snapshotDoc: SnapshotDoc} = await cloudProvider<{snapshotDoc: SnapshotDoc}>();

    return snapshotUserDoc({docId, onNext});
  }

  throw new Error('No publish offline');
};
