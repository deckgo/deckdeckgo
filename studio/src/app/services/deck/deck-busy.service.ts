import {Observable, Subject} from 'rxjs';

export class DeckBusyService {

    private busySubject: Subject<boolean> = new Subject();

    private static instance: DeckBusyService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!DeckBusyService.instance) {
            DeckBusyService.instance = new DeckBusyService();
        }
        return DeckBusyService.instance;
    }

    watch(): Observable<boolean> {
        return this.busySubject.asObservable();
    }

    busy(busy: boolean) {
        this.busySubject.next(busy);
    }
}
