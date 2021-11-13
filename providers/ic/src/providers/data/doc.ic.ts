import {Doc, DocEntries, DocData, DeleteDoc} from '@deckdeckgo/editor';

import {deleteEntry, entries} from '../../utils/data.utils';

export const docEntries: DocEntries = async (_userId: string): Promise<Doc[]> => entries<Doc, DocData>({filter: '/docs/'});

export const deleteDoc: DeleteDoc = async (docId: string): Promise<void> => deleteEntry({key: `/docs/${docId}`});
