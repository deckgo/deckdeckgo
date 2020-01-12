import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckSlides, findSlides} from '../delete/utils/delete-slides-utils';
import {cloneSlides, updateCloneData} from './utils/clone-slides-utils';

import {DeckData} from '../../model/deck';

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

        const deckSlides: DeckSlides | null = await findSlides(deckIdFrom);

        if (!deckSlides) {
            return;
        }

        let slideIds: string[] | undefined = undefined;
        if (deckSlides.slides && deckSlides.slides.length > 0) {
            slideIds = await cloneSlides(deckIdTo, deckSlides.slides);
        }

        await updateCloneData(deckIdTo, slideIds);
        await updateCloneData(deckIdFrom);
    } catch (err) {
        console.error(err);
    }
}
