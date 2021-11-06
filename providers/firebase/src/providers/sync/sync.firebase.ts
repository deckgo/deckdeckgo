import {Sync, SyncData} from '@deckdeckgo/editor';

import {uploadDecks} from '../../utils/sync/sync.deck.utils';
import {uploadDocs} from '../../utils/sync/sync.doc.utils';
import {deleteSlides, uploadSlides} from '../../utils/sync/sync.slide.utils';
import {deleteParagraphs, uploadParagraphs} from '../../utils/sync/sync.paragraphs.utils';

export const sync: Sync = async ({
  syncData,
  userId,
  clean
}: {
  syncData: SyncData | undefined;
  userId: string;
  clean: ({syncedAt}: SyncData) => Promise<void>;
}) => {
  if (!syncData) {
    return;
  }

  // TODO: when we will solve the storage question, we can leverage the data provided as parameter instead of querying idb here again

  const {
    updateDecks,
    updateDocs,
    updateSlides,
    updateParagraphs,
    deleteSlides: deleteSlidesData,
    deleteParagraphs: deleteParagraphsData
  } = syncData;

  // First decks because it contains information for the permission and the list of slides
  await uploadDecks({data: updateDecks, userId});

  await uploadSlides({data: updateSlides, userId});

  await deleteSlides(deleteSlidesData);

  await uploadDocs({data: updateDocs, userId});

  await uploadParagraphs({data: updateParagraphs, userId});

  await deleteParagraphs(deleteParagraphsData);

  await clean(syncData);
};
