import {AuthUser, Doc, SyncState, User} from '@deckdeckgo/editor';
import {AuthStore} from './stores/auth.store';
import {DocStore} from './stores/doc.store';
import {SyncStore} from './stores/sync.store';
import {UserStore} from './stores/user.store';

export * from './events/chart/chart.events';
export * from './events/doc.events';
export * from './events/image/image.load.events';
export * from './loaders/doc.loader';
export * from './providers/auth.provider';
export * from './providers/doc.provider';
export * from './providers/paragraph.provider';
export * from './providers/sync.provider';
export * from './types/env.types';
export * from './utils/before-unload.utils';
export * from './utils/sync.utils';

export const authSubscribe = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().subscribe(callback);
export const docSubscribe = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().subscribe(callback);
export const syncSubscribe = (callback: (syncState: SyncState) => void): (() => void) => SyncStore.getInstance().subscribe(callback);
export const userSubscribe = (callback: (user: User | undefined) => void): (() => void) => UserStore.getInstance().subscribe(callback);
