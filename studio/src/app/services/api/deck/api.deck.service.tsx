import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {Deck} from '../../../models/deck';

import {AuthService} from '../../data/auth/auth.service';

interface DeckPublish {
    url: string;
}

export class ApiDeckService {

    private static instance: ApiDeckService;

    private authService: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!ApiDeckService.instance) {
            ApiDeckService.instance = new ApiDeckService();
        }
        return ApiDeckService.instance;
    }

    post(deck: Deck): Promise<Deck> {
        return this.query(deck, '/decks', 'POST');
    }

    put(deck: Deck, bearer?: string): Promise<Deck> {
        return this.query(deck, `/decks/${deck.id}`, 'PUT', bearer);
    }

    private query(deck: Deck, context: string, method: string, bearer?: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                if (!bearer) {
                    bearer = await this.authService.getBearer();
                }

                const rawResponse: Response = await fetch(apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': bearer
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

    get(deckId: string, bearer?: string): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                if (!bearer) {
                    bearer = await this.authService.getBearer();
                }

                const rawResponse: Response = await fetch(apiUrl + `/decks/${deckId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': bearer
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while loading the deck');
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

    publish(deck: Deck): Promise<string> {
        return new Promise<string>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const bearer: string = await this.authService.getBearer();

                const rawResponse: Response = await fetch(apiUrl + `/decks/${deck.id}/publish`, {
                    method: 'POST',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': bearer
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while publishing the deck');
                    return;
                }
                const result: DeckPublish = await rawResponse.json();

                resolve(result ? result.url : null);
            } catch (err) {
                reject(err);
            }
        });
    }
}

