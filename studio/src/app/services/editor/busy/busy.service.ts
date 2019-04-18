import {Observable, Subject} from 'rxjs';

export class BusyService {

    private deckBusySubject: Subject<boolean> = new Subject();
    private slideEditableSubject: Subject<HTMLElement> = new Subject();

    private static instance: BusyService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!BusyService.instance) {
            BusyService.instance = new BusyService();
        }
        return BusyService.instance;
    }

    watchDeckBusy(): Observable<boolean> {
        return this.deckBusySubject.asObservable();
    }

    deckBusy(busy: boolean) {
        this.deckBusySubject.next(busy);
    }

    watchSlideEditable(): Observable<HTMLElement> {
        return this.slideEditableSubject.asObservable();
    }

    slideEditable(slide: HTMLElement) {
        this.slideEditableSubject.next(slide);
    }
}
