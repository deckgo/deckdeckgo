import {SyncData} from '../types/sync';

export interface Sync {
  ({
    syncData,
    userId,
    clean
  }: {
    syncData: SyncData | undefined;
    userId: string;
    clean: ({syncedAt}: SyncData) => Promise<void>;
  }): Promise<void>;
}
