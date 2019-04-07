import {Deck} from '../../models/deck';

export class DeckEditorService {
    
    deck: Deck;

    private static instance: DeckEditorService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!DeckEditorService.instance) {
            DeckEditorService.instance = new DeckEditorService();
        }
        return DeckEditorService.instance;
    }
    
}
