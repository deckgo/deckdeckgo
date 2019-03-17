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
        return this.postOrPut(deck, 'POST');
    }

    put(deck: Deck): Promise<Deck> {
        return this.postOrPut(deck, 'PUT');
    }

    private postOrPut(deck: Deck, method: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + '/decks', {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(deck)
                });

                const persistedDeck: Deck = await rawResponse.json();

                console.log(persistedDeck);

                resolve(persistedDeck);
            } catch (err) {
                reject(err);
            }
        });
    }
}

