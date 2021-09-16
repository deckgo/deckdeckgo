import {StorageFile, StorageFilesList, StorageFoldersList} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';

import {StorageIcProvider} from './storage.ic.provider';
import {StorageOfflineProvider} from './storage.offline.provider';

import {firebase, internetComputer} from '../../utils/core/environment.utils';

import {Constants} from '../../types/core/constants';

export const uploadOnlineFile = async (
  data: File,
  folder: string,
  maxSize: number,
  downloadUrl?: boolean
): Promise<StorageFile | undefined> => {
  if (internetComputer()) {
    return StorageIcProvider.getInstance().uploadFile(data, folder, maxSize);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {updateFile} = await import(cdn);

    return updateFile({
      data,
      folder,
      maxSize,
      downloadUrl,
      userId: authStore.state.authUser.uid
    });
  }

  throw new Error('No provider to upload file online.');
};

export const getFiles = async (next: string | null, folder: string): Promise<StorageFilesList | null> => {
  if (internetComputer()) {
    return StorageIcProvider.getInstance().getFiles(next, folder);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {getFiles} = await import(cdn);

    return getFiles({
      next,
      maxResults: Constants.STORAGE.MAX_QUERY_RESULTS,
      folder,
      userId: authStore.state.authUser.uid
    });
  }

  return StorageOfflineProvider.getInstance().getFiles(next, folder);
};

export const getFolders = async (folder: string): Promise<StorageFoldersList | undefined> => {
  if (internetComputer()) {
    return StorageIcProvider.getInstance().getFolders(folder);
  }

  if (firebase()) {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {getFolders} = await import(cdn);

    return getFolders({
      folder,
      userId: authStore.state.authUser.uid
    });
  }

  return StorageOfflineProvider.getInstance().getFolders(folder);
};
