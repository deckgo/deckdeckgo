import {AuthUser, Doc, SyncState, User} from '@deckdeckgo/editor';
import {initSyncState} from './providers/sync.provider';
import {AuthStore} from './stores/auth.store';
import {DocStore} from './stores/doc.store';
import {EnvStore} from './stores/env.store';
import {SyncStore} from './stores/sync.store';
import {UserStore} from './stores/user.store';
import {EnvironmentCloud} from './types/env.types';

export * from './events/doc.events';
export * from './events/image/image.load.events';
export * from './events/chart/chart.events';
export * from './providers/auth.provider';
export * from './providers/sync.provider';
export * from './providers/doc.provider';
export * from './providers/paragraph.provider';
export * from './types/env.types';
export * from './utils/sync.utils';
export * from './utils/before-unload.utils';
export * from './loaders/doc.loader';

export const authSubscribe = (callback: (authUser: AuthUser | null) => void): (() => void) => AuthStore.getInstance().subscribe(callback);
export const docSubscribe = (callback: (doc: Doc | null) => void): (() => void) => DocStore.getInstance().subscribe(callback);
export const syncSubscribe = (callback: (authUser: SyncState) => void): (() => void) => SyncStore.getInstance().subscribe(callback);
export const userSubscribe = (callback: (user: User | undefined) => void): (() => void) => UserStore.getInstance().subscribe(callback);

export const initSync = ({env}: {env: EnvironmentCloud | undefined}): (() => void) => {
  EnvStore.getInstance().set(env);

  return AuthStore.getInstance().subscribe(async (authUser: AuthUser | null | undefined) => {
    initSyncState().then(() => {});
  });
};
