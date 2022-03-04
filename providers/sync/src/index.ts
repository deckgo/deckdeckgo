import {AuthUser, Deck, Doc, SyncState, User} from '@deckdeckgo/editor';
import {AuthStore} from './stores/auth.store';
import {DeckStore} from './stores/deck.store';
import {DocStore} from './stores/doc.store';
import {SyncStore} from './stores/sync.store';
import {UserStore} from './stores/user.store';

export * from './events/chart/chart.events';
export * from './events/doc.events';
export * from './events/image/image.load.events';
export * from './init/sync.init';
export * from './loaders/deck.loader';
export * from './loaders/doc.loader';
export * from './providers/auth.provider';
export * from './providers/deck.provider';
export * from './providers/doc.provider';
export * from './providers/paragraph.provider';
export * from './providers/publish.provider';
export * from './providers/slide.provider';
export * from './providers/storage.provider';
export * from './providers/sync.provider';
export * from './providers/user.provider';
export * from './services/admin.service';
export * from './services/file-system.service';
export * from './types/env.types';
export * from './types/import.types';
export * from './types/publish.types';
export * from './utils/before-unload.utils';
export {importEditorData, importEditorSync} from './utils/import.utils';
// Export sync functions only for deck because deck.data.events remains at the moment in the studio editor
export {syncDeleteDeck, syncDeleteSlide, syncUpdateDeck, syncUpdateSlide} from './utils/sync.utils';

export const authSubscribe = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().subscribe(callback);
export const docSubscribe = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().subscribe(callback);
export const deckSubscribe = (callback: (deck: Deck | null) => void): (() => void) => DeckStore.getInstance().subscribe(callback);
export const syncSubscribe = (callback: (syncState: SyncState) => void): (() => void) => SyncStore.getInstance().subscribe(callback);
export const userSubscribe = (callback: (user: User | undefined) => void): (() => void) => UserStore.getInstance().subscribe(callback);

// Export update store functions only for deck because deck.data.events remains at the moment in the studio editor
export const setDeck = (deck: Deck | null) => DeckStore.getInstance().set(deck);
