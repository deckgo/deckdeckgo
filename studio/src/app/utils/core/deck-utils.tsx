import {Deck} from '../../models/data/deck';

export class DeckUtils {

    static filterDecks(filter: string, decks: Deck[]): Promise<Deck[]> {
        return new Promise<Deck[]>((resolve) => {
            if (!filter || filter === undefined || filter === '') {
                const results: Deck[] = decks ? [...decks] : null;

                resolve(results);
                return;
            }

            if (!decks || decks.length <= 0) {
                const results: Deck[] = decks ? [...decks] : null;

                resolve(results);
                return;
            }

            const matchingDecks: Deck[] = decks.filter((matchDeck: Deck) => {
                return matchDeck.data && matchDeck.data.name && matchDeck.data.name.toLowerCase().indexOf(filter.toLowerCase()) > -1
            });

            const results: Deck[] = [...matchingDecks];

            resolve(results);
        });
    }
}
