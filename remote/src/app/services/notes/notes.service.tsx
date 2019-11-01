import {BehaviorSubject, Observable} from 'rxjs';

import {DeckdeckgoSlideDefinition} from '@deckdeckgo/types';

export class NotesService {

    private static instance: NotesService;

    private currentSlide: BehaviorSubject<DeckdeckgoSlideDefinition> = new BehaviorSubject<DeckdeckgoSlideDefinition>(null);

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!NotesService.instance) {
            NotesService.instance = new NotesService();
        }
        return NotesService.instance;
    }

    watch(): Observable<DeckdeckgoSlideDefinition> {
        return this.currentSlide.asObservable();
    }

    nextSlideDefinition(slide: DeckdeckgoSlideDefinition) {
        this.currentSlide.next(slide);
    }

}
