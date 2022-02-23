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

export const onAuthChange = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().onChange(callback);
export const onDocChange = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().onChange(callback);
export const onSyncChange = (callback: (authUser: SyncState) => void): (() => void) => SyncStore.getInstance().onChange(callback);
export const onErrorChange = (callback: (error: string | undefined) => void): (() => void) => ErrorStore.getInstance().onChange(callback);
export const onBusyChange = (callback: (busy: boolean) => void): (() => void) => BusyStore.getInstance().onChange(callback);
