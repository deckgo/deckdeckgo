import {ApiDeckService} from '../deck/api.deck.service';
import {ApiDeck} from '../../../models/api/api.deck';

export class ApiMergeService {
    
    private static instance: ApiMergeService;

    private deckService: ApiDeckService;

    private constructor() {
        // Private constructor, singleton
        this.deckService = ApiDeckService.getInstance();
    }

    static getInstance() {
        if (!ApiMergeService.instance) {
            ApiMergeService.instance = new ApiMergeService();
        }
        return ApiMergeService.instance;
    }

    mergeDeck(deckId: string, userToken: string, newUserId: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!deckId || !userToken || !newUserId) {
                resolve();
                return;
            }

            const bearer: string = `Bearer ${userToken}`;

            const deck: ApiDeck = await this.deckService.get(deckId, bearer);

            if (!deck) {
                resolve();
                return;
            }

            deck.owner_id = newUserId;

            await this.deckService.put(deck, bearer);

            resolve();
        });
    }

    
}
