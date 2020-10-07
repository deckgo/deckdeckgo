import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckSlides, deleteSlides, findSlides} from './utils/delete-slides-utils';

export async function deleteDeckSlides(snap: DocumentSnapshot, context: EventContext) {
  const deckId: string = context.params.deckId;

  if (!deckId || deckId === undefined || deckId === '') {
    return;
  }

  try {
    const deckSlides: DeckSlides | null = await findSlides(deckId);

    if (!deckSlides) {
      return;
    }

    await deleteSlides(deckId, deckSlides.slides);
  } catch (err) {
    console.error(err);
  }
}
