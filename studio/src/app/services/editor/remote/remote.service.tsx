import {BehaviorSubject, Observable} from 'rxjs';

import {get, set} from 'idb-keyval';
import {filter, take} from 'rxjs/operators';
import {Deck} from '../../../models/deck';
import {DeckEditorService} from '../../api/deck/deck-editor.service';

export class RemoteService {

    private remoteSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);

    private static instance: RemoteService;

    private deckEditorService: DeckEditorService;

    private constructor() {
        // Private constructor, singleton
        this.deckEditorService = DeckEditorService.getInstance();
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

    getRoom(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.deckEditorService.watch().pipe(filter((deck: Deck) => deck && (deck.name && deck.name !== undefined && deck.name !== '')), take(1)).subscribe(async (deck: Deck) => {
                const roomName: string = deck.name.replace(/\./g,'_');

                resolve(roomName);
            });
        });
    }
}
