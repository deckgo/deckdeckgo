import {EnvironmentConfigService} from '../environment/environment-config.service';

import {Deck} from '../../models/deck';

export class DeckService {

    private static instance: DeckService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!DeckService.instance) {
            DeckService.instance = new DeckService();
        }
        return DeckService.instance;
    }

    post(deck: Deck): Promise<Deck> {
        return this.postOrPut(deck, '/decks', 'POST');
    }

    put(deck: Deck): Promise<Deck> {
        return this.postOrPut(deck, '/decks/' + deck.deck_id, 'PUT');
    }

    private postOrPut(deck: Deck, context: string, method: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(deck)
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating or updating the deck');
                    return;
                }

                const persistedDeck: Deck = await rawResponse.json();

                resolve(persistedDeck);
            } catch (err) {
                reject(err);
            }
        });
    }
}

