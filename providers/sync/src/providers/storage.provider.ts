import {DeleteFile, GetFiles, StorageFile, StorageFilesList, UploadFile} from '@deckdeckgo/editor';
import {getOfflineFiles} from '@deckdeckgo/offline';
import {STORAGE_MAX_QUERY_RESULTS} from '../constants/storage.constants';
import {AuthStore} from '../stores/auth.store';
import {EnvStore} from '../stores/env.store';
import {isOnline} from '../utils/offline.utils';
import {cloudProvider} from '../utils/providers.utils';

export const uploadOnlineFile = async (
  data: File,
  folder: string,
  maxSize: number,
  downloadUrl?: boolean
): Promise<StorageFile | undefined> => {
  if (EnvStore.getInstance().cloud()) {
    const {uploadFile}: {uploadFile: UploadFile} = await cloudProvider<{uploadFile: UploadFile}>();

    return uploadFile({
      data,
      folder,
      maxSize,
      downloadUrl,
      userId: AuthStore.getInstance().get().uid
    });
  }

  throw new Error('No provider to upload file online.');
};

export const getFiles = async ({next, folder}: {next: string | null; folder: string}): Promise<StorageFilesList | null> => {
  if (!AuthStore.getInstance().isLoggedIn() || !isOnline()) {
    return getOfflineFiles(folder);
  }

  if (EnvStore.getInstance().cloud()) {
    const {getFiles}: {getFiles: GetFiles} = await cloudProvider<{getFiles: GetFiles}>();

    return getFiles({
      next,
      maxResults: STORAGE_MAX_QUERY_RESULTS,
      folder,
      userId: AuthStore.getInstance().get().uid
    });
  }

  return getOfflineFiles(folder);
};

export const deleteFile = async (storageFile: StorageFile) => {
  if (EnvStore.getInstance().cloud()) {
    const {deleteFile}: {deleteFile: DeleteFile} = await cloudProvider<{deleteFile: DeleteFile}>();

    return deleteFile(storageFile);
  }

  throw new Error('No provider to delete online file.');
};
