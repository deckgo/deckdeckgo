import {createStore} from '@stencil/store';

import {Doc} from '@deckdeckgo/editor';

import {setEditDocId} from '../utils/editor/editor.utils';

interface DocStore {
  doc: Doc | null;
}

const {state, onChange, reset} = createStore<DocStore>({
  doc: null
});

onChange('doc', (doc: Doc | null) => {
  if (!doc) {
    return;
  }

  setEditDocId(doc.id).catch((err) => {
    console.error('Failed to update IDB with new doc id', err);
  });
});

export default {state, onChange, reset};
