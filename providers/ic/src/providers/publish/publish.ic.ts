import {Principal} from '@dfinity/principal';

import {Deck, Publish, PublishUrl} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {publishDeck} from '../../utils/publish.deck.utils';
import {uploadResources} from '../../utils/publish.resources.utils';
import {getPublishBucket} from '../../utils/publish.utils';

export const publish: Publish = async ({deck}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  await uploadResources({deck});

  return await publishDeck({deck});
};

export const publishUrl: PublishUrl = async () => {
  const {bucket}: {bucket: Principal; actor: StorageBucketActor} = await getPublishBucket();
  return `https://${bucket.toText()}.raw.ic0.app`;
};
