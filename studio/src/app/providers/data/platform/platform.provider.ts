import {Token, MergeToken} from '@deckdeckgo/editor';

import errorStore from '../../../stores/error.store';

import {firebase} from '../../../utils/core/environment.utils';

export const mergePlatformToken = async (token: Token) => {
  if (!token) {
    return;
  }

  // TODO: Platform token for Internet Computer?

  if (!firebase()) {
    throw new Error('Merge token not supported');
  }

  try {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {mergeToken}: {mergeToken: MergeToken} = await import(cdn);

    await mergeToken(token);
  } catch (err) {
    errorStore.state.error = 'GitHub platform information not properly set up.';
  }
};
