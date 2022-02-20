import {MergeToken, Token} from '@deckdeckgo/editor';
import {errorStore} from '@deckdeckgo/studio';
import {firebase} from '../../../utils/core/environment.utils';
import {cloudProvider} from '../../../utils/core/providers.utils';

export const mergePlatformToken = async (token: Token) => {
  if (!token) {
    return;
  }

  // TODO: Platform token for Internet Computer?

  if (!firebase()) {
    throw new Error('Merge token not supported');
  }

  try {
    const {mergeToken}: {mergeToken: MergeToken} = await cloudProvider<{mergeToken: MergeToken}>();

    await mergeToken(token);
  } catch (err) {
    errorStore.default.state.error = 'GitHub platform information not properly set up.';
  }
};
