import {EnvironmentConfigService} from '../environment/environment-config.service';

import {Deck} from '../../models/deck';

import {AuthService} from '../auth/auth.service';

export class DeckService {

    private static instance: DeckService;

    private authService: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!DeckService.instance) {
            DeckService.instance = new DeckService();
        }
        return DeckService.instance;
    }

    post(deck: Deck): Promise<Deck> {
        return this.query(deck, '/decks', 'POST');
    }

    put(deck: Deck): Promise<Deck> {
        return this.query(deck, '/decks/' + deck.deck_id, 'PUT');
    }

    private query(deck: Deck, context: string, method: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
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

    get(deckId: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/decks/${deckId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while search for the deck');
                    return;
                }

                const deck: Deck = await rawResponse.json();

                resolve(deck);
            } catch (err) {
                reject(err);
            }
        });
    }

    getUserDecks(userId: string): Promise<Deck[]> {
        return new Promise<Deck[]>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + '/decks/?owner_id=' + userId, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating or updating the deck');
                    return;
                }

                const persistedDecks: Deck[] = await rawResponse.json();

                resolve(persistedDecks);
            } catch (err) {
                reject(err);
            }
        });
    }
}

