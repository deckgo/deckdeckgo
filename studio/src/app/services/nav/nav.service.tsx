import {Observable, Subject} from 'rxjs';

export class NavService {

    private navSubject: Subject<string> = new Subject();

    private static instance: NavService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!NavService.instance) {
            NavService.instance = new NavService();
        }
        return NavService.instance;
    }

    watch(): Observable<string> {
        return this.navSubject.asObservable();
    }

    navigate(url: string) {
        this.navSubject.next(url);
    }
}
