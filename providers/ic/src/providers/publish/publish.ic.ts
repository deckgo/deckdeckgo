import {Deck, DeckPublish, Doc, DocPublish, PublishUrl} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {publishDeck} from '../../utils/publish.deck.utils';
import {uploadResources} from '../../utils/publish.resources.utils';
import {publishOverview} from '../../utils/publish.overview.utils';
import {getStorageActor} from '../../utils/storage.utils';
import {BucketActor} from '../../utils/manager.utils';

export const deckPublish: DeckPublish = async ({deck}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  await uploadResources({deck});

  const {storageUpload, deckPublishData, deck: updatedDeck} = await publishDeck({deck});

  await publishOverview({storageUpload, deckPublishData, deckId: updatedDeck.id});

  return updatedDeck;
};

export const docPublish: DocPublish = async ({doc}: {doc: Doc}): Promise<Doc> => {
  // TODO
  console.log(doc);
  throw new Error('Not yet implemented');
};

export const publishUrl: PublishUrl = async () => {
  const {bucketId}: BucketActor<StorageBucketActor> = await getStorageActor();
  return `https://${bucketId.toText()}.raw.ic0.app`;
};
