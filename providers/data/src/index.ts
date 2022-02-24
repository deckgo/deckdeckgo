import {AuthUser, Doc, SyncState, User} from '@deckdeckgo/editor';
import {AuthStore} from './stores/auth.store';
import {BusyStore} from './stores/busy.store';
import {DocStore} from './stores/doc.store';
import {ErrorStore} from './stores/error.store';
import {SyncStore} from './stores/sync.store';
import {UserStore} from './stores/user.store';

export * from './events/doc.events';
export * from './providers/auth.provider';
export * from './providers/sync.provider';
export * from './types/env.types';

export const authSubscribe = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().subscribe(callback);
export const docSubscribe = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().subscribe(callback);
export const syncSubscribe = (callback: (authUser: SyncState) => void): (() => void) => SyncStore.getInstance().subscribe(callback);
export const errorSubscribe = (callback: (error: string | undefined) => void): (() => void) => ErrorStore.getInstance().subscribe(callback);
export const busySubscribe = (callback: (busy: boolean) => void): (() => void) => BusyStore.getInstance().subscribe(callback);
export const userSubscribe = (callback: (user: User | undefined) => void): (() => void) => UserStore.getInstance().subscribe(callback);
