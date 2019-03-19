import {Observable, Subject} from 'rxjs';

export class ErrorService {

    private errorSubject: Subject<string> = new Subject();

    private static instance: ErrorService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!ErrorService.instance) {
            ErrorService.instance = new ErrorService();
        }
        return ErrorService.instance;
    }

    watch(): Observable<string> {
        return this.errorSubject.asObservable();
    }

    error(error: string) {
        this.errorSubject.next(error);
    }
}
