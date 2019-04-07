import {DeckService} from '../deck/deck.service';
import {Deck} from '../../models/deck';

export class MergeService {
    
    private static instance: MergeService;

    private deckService: DeckService;

    private constructor() {
        // Private constructor, singleton
        this.deckService = DeckService.getInstance();
    }

    static getInstance() {
        if (!MergeService.instance) {
            MergeService.instance = new MergeService();
        }
        return MergeService.instance;
    }

    mergeDeck(deckId: string, userToken: string, newUserId: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!deckId || !userToken || !newUserId) {
                resolve();
                return;
            }

            const bearer: string = `Bearer ${userToken}`;

            const deck: Deck = await this.deckService.get(deckId, bearer);

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
