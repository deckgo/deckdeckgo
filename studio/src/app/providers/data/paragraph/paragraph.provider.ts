import {GetParagraph, Paragraph} from '@deckdeckgo/editor';
import {getOfflineParagraph} from '@deckdeckgo/studio';
import {cloud} from '../../../utils/core/environment.utils';
import {cloudProvider} from '../../../utils/core/providers.utils';

export const getParagraph = async ({docId, paragraphId}: {docId: string; paragraphId: string}): Promise<Paragraph> => {
  if (cloud()) {
    const {getParagraph: getUserParagraph}: {getParagraph: GetParagraph} = await cloudProvider<{getParagraph: GetParagraph}>();

    return getUserParagraph(docId, paragraphId);
  }

  return getOfflineParagraph({docId, paragraphId});
};
