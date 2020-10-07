import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {cloneSlides, updateCloneData} from './utils/clone-slides-utils';

import {DeckData} from '../../model/data/deck';

export async function cloneDeckSlides(snap: DocumentSnapshot, context: EventContext) {
  const deck: DeckData = snap.data() as DeckData;

  if (!deck || deck === undefined) {
    return;
  }

  if (!deck.clone || deck.clone === undefined || !deck.clone.deck_id_from || deck.clone.deck_id_from === undefined || deck.clone.deck_id_from === '') {
    return;
  }

  const deckIdTo: string = context.params.deckId;

  if (!deckIdTo || deckIdTo === undefined || deckIdTo === '') {
    return;
  }

  try {
    const deckIdFrom: string = deck.clone.deck_id_from;

    const slideIds: string[] | undefined = await cloneSlides(deckIdTo, deckIdFrom);

    await updateCloneData(deckIdTo, slideIds);
    await updateCloneData(deckIdFrom);
  } catch (err) {
    console.error(err);
  }
}
