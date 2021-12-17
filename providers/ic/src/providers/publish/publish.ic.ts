import {Deck, DeckPublish, Doc, DocPublish, PublishUrl} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {BucketActor} from '../../utils/manager.utils';
import {publishDeck} from '../../utils/publish.deck.utils';
import {uploadResources} from '../../utils/publish.resources.utils';
import {publishOverview} from '../../utils/publish.overview.utils';
import {getStorageActor} from '../../utils/storage.utils';
import {publishDoc} from '../../utils/publish.doc.utils';

export const deckPublish: DeckPublish = async ({deck}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  await uploadResources({meta: deck.data.meta});

  const {storageUpload, publishData, deck: updatedDeck} = await publishDeck({deck});

  await publishOverview({storageUpload, publishData, dataId: updatedDeck.id});

  return updatedDeck;
};

export const docPublish: DocPublish = async ({doc}: {doc: Doc}): Promise<Doc> => {
  await uploadResources({meta: doc.data.meta});

  const {storageUpload, publishData, doc: updatedDoc} = await publishDoc({doc});

  await publishOverview({storageUpload, publishData, dataId: updatedDoc.id});

  return updatedDoc;
};

export const publishUrl: PublishUrl = async () => {
  const {bucketId}: BucketActor<StorageBucketActor> = await getStorageActor();
  return `https://${bucketId.toText()}.raw.ic0.app`;
};
