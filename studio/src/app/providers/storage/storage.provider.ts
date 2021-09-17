import {GetFiles, GetFolders, StorageFile, StorageFilesList, StorageFoldersList, UploadFile} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';

import {StorageIcProvider} from './storage.ic.provider';
import {StorageOfflineProvider} from './storage.offline.provider';

import {firebase, internetComputer} from '../../utils/core/environment.utils';
import {provider} from '../../utils/core/providers.utils';

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
    const {uploadFile}: {uploadFile: UploadFile} = await provider<{uploadFile: UploadFile}>();

    return uploadFile({
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
    const {getFiles}: {getFiles: GetFiles} = await provider<{getFiles: GetFiles}>();

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
    const {getFolders}: {getFolders: GetFolders} = await provider<{getFolders: GetFolders}>();

    return getFolders({
      folder,
      userId: authStore.state.authUser.uid
    });
  }

  return StorageOfflineProvider.getInstance().getFolders(folder);
};
