import {DeleteDoc, Doc, DocData, DocEntries, SnapshotDoc} from '@deckdeckgo/editor';
import {deleteData, entries} from '../../utils/data.utils';

export const docEntries: DocEntries = async (_userId: string): Promise<Doc[]> =>
  entries<Doc, DocData>({startsWith: '/docs/', notContains: '/paragraphs/'});

export const deleteDoc: DeleteDoc = async (docId: string): Promise<void> => deleteData({key: `/docs/${docId}`});

// Backwards compatibility with current publish mask in studio and Firebase support. In case of IC we actually do not need a snapshot, publish is synchronous on the client side.
export const snapshotDoc: SnapshotDoc = async ({
  onNext
}: {
  docId: string;
  onNext: (snapshot: Doc) => Promise<void>;
  onError?: (error: string) => void;
}): Promise<() => void | undefined> => {
  document.addEventListener('docPublished', async ({detail}: CustomEvent<Doc>) => await onNext(detail), {passive: true});

  return () => document.removeEventListener('docPublished', ({detail}: CustomEvent<Doc>) => onNext(detail), false);
};
