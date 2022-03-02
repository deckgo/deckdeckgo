import {AuthUser, Deck, Doc} from '@deckdeckgo/editor';
import {setEditDeckId, setEditDocId} from '@deckdeckgo/offline';
import {initSyncState} from '../providers/sync.provider';
import {AuthStore} from '../stores/auth.store';
import {DeckStore} from '../stores/deck.store';
import {DocStore} from '../stores/doc.store';
import {EnvStore} from '../stores/env.store';
import {Environment} from '../types/env.types';

export const initSync = ({env}: {env: Environment | undefined}): (() => void)[] => {
  EnvStore.getInstance().set(env);

  const docUnsubscriber: () => void = DocStore.getInstance().subscribe((doc: Doc | null) => {
    if (!doc) {
      return;
    }

    setEditDocId(doc.id).catch((err) => {
      console.error('Failed to update IDB with new doc id', err);
    });
  });

  const deckUnsubscriber: () => void = DeckStore.getInstance().subscribe((deck: Deck | null) => {
    if (!deck) {
      return;
    }

    setEditDeckId(deck.id).catch((err) => {
      console.error('Failed to update IDB with new deck id', err);
    });
  });

  const authUnsubscriber: () => void = AuthStore.getInstance().subscribe(async (authUser: AuthUser | null | undefined) =>
    initSyncState().then(() => {})
  );

  return [docUnsubscriber, deckUnsubscriber, authUnsubscriber];
};
