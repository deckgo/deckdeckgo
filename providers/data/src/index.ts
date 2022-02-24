import {AuthUser, Doc, SyncState} from '@deckdeckgo/editor';
import {AuthStore} from './stores/auth.store';
import {BusyStore} from './stores/busy.store';
import {DocStore} from './stores/doc.store';
import {ErrorStore} from './stores/error.store';
import {SyncStore} from './stores/sync.store';

export * from './events/doc.events';
export * from './stores/auth.store';
export * from './stores/busy.store';
export * from './stores/doc.store';
export * from './stores/error.store';
export * from './stores/sync.store';
export * from './types/env.types';

export const authSubscribe = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().subscribe(callback);
export const docSubscribe = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().subscribe(callback);
export const syncSubscribe = (callback: (authUser: SyncState) => void): (() => void) => SyncStore.getInstance().subscribe(callback);
export const errorSubscribe = (callback: (error: string | undefined) => void): (() => void) => ErrorStore.getInstance().subscribe(callback);
export const busySubscribe = (callback: (busy: boolean) => void): (() => void) => BusyStore.getInstance().subscribe(callback);
