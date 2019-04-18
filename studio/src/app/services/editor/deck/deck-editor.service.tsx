import {BehaviorSubject, Observable} from 'rxjs';

import {Deck} from '../../../models/deck';

export class DeckEditorService {

    private deckSubject: BehaviorSubject<Deck> = new BehaviorSubject(null);

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

    watch(): Observable<Deck> {
        return this.deckSubject.asObservable();
    }

    next(deck: Deck) {
        this.deckSubject.next(deck);
    }
    
}
