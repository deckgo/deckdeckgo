import {Deck, Publish, PublishUrl} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {publishDeck} from '../../utils/publish.deck.utils';
import {uploadResources} from '../../utils/publish.resources.utils';
import {publishOverview} from '../../utils/publish.overview.utils';
import {getStorageActor} from '../../utils/storage.utils';
import {BucketActor} from '../../utils/manager.utils';

export const publish: Publish = async ({deck}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  await uploadResources({deck});

  const {storageUpload, deckPublishData, deck: updatedDeck} = await publishDeck({deck});

  await publishOverview({storageUpload, deckPublishData, deckId: updatedDeck.id});

  return updatedDeck;
};

export const publishUrl: PublishUrl = async () => {
  const {bucketId}: BucketActor<StorageBucketActor> = await getStorageActor();
  return `https://${bucketId.toText()}.raw.ic0.app`;
};
