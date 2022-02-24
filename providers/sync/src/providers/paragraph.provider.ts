import {GetParagraph, Paragraph} from '@deckdeckgo/editor';
import {getOfflineParagraph} from '@deckdeckgo/offline';
import { EnvStore } from '../stores/env.store';
import { cloudProvider } from '../utils/providers.utils';

export const getParagraph = async ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<Paragraph> => {
  if (EnvStore.getInstance().cloud()) {
    const {getParagraph: getUserParagraph}: {getParagraph: GetParagraph} = await cloudProvider<{getParagraph: GetParagraph}>();

    return getUserParagraph(docId, paragraphId);
  }

  return getOfflineParagraph({docId, paragraphId});
};
