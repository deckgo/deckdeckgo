import {Deck, Publish} from '@deckdeckgo/editor';

import {publishDeck} from '../../utils/publish.deck.utils';
import {uploadResources} from '../../utils/publish.resources.utils';

export const publish: Publish = async ({deck}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  await uploadResources({deck});

  return await publishDeck({deck});
};
