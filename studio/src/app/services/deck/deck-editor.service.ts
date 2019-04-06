import {BehaviorSubject, Observable} from 'rxjs';

export class DeckEditorService {
    
    private deckIdSubject: BehaviorSubject<string> = new BehaviorSubject(null);

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

    watch(): Observable<string> {
        return this.deckIdSubject.asObservable();
    }

    deckEdited(deckId: string) {
        this.deckIdSubject.next(deckId);
    }

    updateEditorUrl(deckId: string) {
        history.replaceState({}, `Deck edited ${deckId}`, `/editor/${deckId}`);
    }
    
}
