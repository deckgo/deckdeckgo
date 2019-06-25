import {BehaviorSubject, Observable} from 'rxjs';

import {ApiDeck} from '../../../models/api/api.deck';

export class DeckEditorService {

    private deckSubject: BehaviorSubject<ApiDeck> = new BehaviorSubject(null);

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

    watch(): Observable<ApiDeck> {
        return this.deckSubject.asObservable();
    }

    next(deck: ApiDeck) {
        this.deckSubject.next(deck);
    }
    
}
