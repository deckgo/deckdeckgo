import {BehaviorSubject, Observable} from 'rxjs';

import {get, set} from 'idb-keyval';
import {take} from 'rxjs/operators';

export class RemoteService {

    private remoteSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);

    private static instance: RemoteService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!RemoteService.instance) {
            RemoteService.instance = new RemoteService();
        }
        return RemoteService.instance;
    }

    init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const remote: boolean = await get('deckdeckgo_remote');

            this.watch().pipe(take(1)).subscribe((current: boolean) => {
               if (current !== remote) {
                   this.remoteSubject.next(remote);
               }
            });

            resolve();
        });
    }

    async switch(enable: boolean) {
        await set('deckdeckgo_remote', enable);
        this.remoteSubject.next(enable);
    }

    watch(): Observable<boolean> {
        return this.remoteSubject.asObservable();
    }
}
