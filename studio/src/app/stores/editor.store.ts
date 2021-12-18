import {createStore} from '@stencil/store';

import {Deck, Doc} from '@deckdeckgo/editor';

import {setEditDeckId, setEditDocId} from '../utils/editor/editor.utils';

interface EditorStore {
  doc: Doc | null;
  deck: Deck | null;
  name: string | null;
  published: boolean;
}

const {state, onChange, reset} = createStore<EditorStore>({
  doc: null,
  deck: null,
  name: null,
  published: false
});

onChange('name', (name: string | null) => {
  document.title = name ?? 'DeckDeckGo';
});

onChange('deck', (deck: Deck | null) => {
  state.name = deck?.data?.name && deck?.data?.name !== '' ? deck.data.name : null;
  state.published = deck?.data?.meta?.published ?? false;

  if (!deck) {
    return;
  }

  state.doc = null;

  setEditDeckId(deck.id).catch((err) => {
    console.error('Failed to update IDB with new deck id', err);
  });
});

onChange('doc', (doc: Doc | null) => {
  state.name = doc?.data?.name && doc?.data?.name !== '' ? doc.data.name : null;
  state.published = doc?.data?.meta?.published ?? false;

  if (!doc) {
    return;
  }

  state.deck = null;

  setEditDocId(doc.id).catch((err) => {
    console.error('Failed to update IDB with new doc id', err);
  });
});

export default {state, onChange, reset};
