import {Observable, Subject} from 'rxjs';

export enum NavDirection {
    FORWARD,
    ROOT,
    BACK
}

export interface NavParams {
    url?: string;
    direction: NavDirection
}

export class NavService {

    private navSubject: Subject<NavParams> = new Subject();

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

    watch(): Observable<NavParams> {
        return this.navSubject.asObservable();
    }

    navigate(params: NavParams) {
        this.navSubject.next(params);
    }
}
