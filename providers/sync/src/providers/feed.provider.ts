import {Deck, DeckSubmitFeed, Doc, DocSubmitFeed, throwError} from '@deckdeckgo/editor';
import {DeckStore} from '../stores/deck.store';
import {DocStore} from '../stores/doc.store';
import {EnvStore} from '../stores/env.store';
import {cloudProvider} from '../utils/providers.utils';

export const submitFeed = async () => {
  if (!EnvStore.getInstance().cloud()) {
    throwError('Submit to feed is only available with a compatible cloud provider.');
    return;
  }

  if (DocStore.getInstance().get() !== null && DocStore.getInstance().get() !== undefined) {
    await submitFeedDoc();
    return;
  }

  if (DeckStore.getInstance().get() !== null && DeckStore.getInstance().get() !== undefined) {
    await submitFeedDeck();
    return;
  }

  throwError('No data provided to submit to feed.');
};

const submitFeedDoc = async () => {
  const doc: Doc = {...DocStore.getInstance().get()};

  const {docSubmitFeed}: {docSubmitFeed: DocSubmitFeed} = await cloudProvider<{docSubmitFeed: DocSubmitFeed}>();

  await docSubmitFeed({doc});
};

const submitFeedDeck = async () => {
  const deck: Deck = {...DeckStore.getInstance().get()};

  const {deckSubmitFeed}: {deckSubmitFeed: DeckSubmitFeed} = await cloudProvider<{deckSubmitFeed: DeckSubmitFeed}>();

  await deckSubmitFeed({deck});
};
