import {BehaviorSubject, Observable} from 'rxjs';

export class NotesService {

    private static instance: NotesService;

    private currentSlide: BehaviorSubject<HTMLElement> = new BehaviorSubject<HTMLElement>(null);

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!NotesService.instance) {
            NotesService.instance = new NotesService();
        }
        return NotesService.instance;
    }

    watch(): Observable<HTMLElement> {
        return this.currentSlide.asObservable();
    }

    next(slide: HTMLElement) {
        this.currentSlide.next(slide);
    }

}
