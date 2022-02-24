import {DeleteFile, GetFiles, StorageFile, StorageFilesList, UploadFile} from '@deckdeckgo/editor';
import {Constants} from '../../config/constants';
import authStore from '../../stores/auth.store';
import offlineStore from '../../stores/offline.store';
import {cloud} from '../../utils/core/environment.utils';
import {cloudProvider} from '../../utils/core/providers.utils';
import {StorageOfflineProvider} from './storage.offline.provider';

export const uploadOnlineFile = async (
  data: File,
  folder: string,
  maxSize: number,
  downloadUrl?: boolean
): Promise<StorageFile | undefined> => {
  if (cloud()) {
    const {uploadFile}: {uploadFile: UploadFile} = await cloudProvider<{uploadFile: UploadFile}>();

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

export const getFiles = async ({next, folder}: {next: string | null; folder: string}): Promise<StorageFilesList | null> => {
  if (!authStore.state.loggedIn || !offlineStore.state.online) {
    return StorageOfflineProvider.getInstance().getFiles(folder);
  }

  if (cloud()) {
    const {getFiles}: {getFiles: GetFiles} = await cloudProvider<{getFiles: GetFiles}>();

    return getFiles({
      next,
      maxResults: Constants.STORAGE.MAX_QUERY_RESULTS,
      folder,
      userId: authStore.state.authUser.uid
    });
  }

  return StorageOfflineProvider.getInstance().getFiles(folder);
};

export const deleteFile = async (storageFile: StorageFile) => {
  if (cloud()) {
    const {deleteFile}: {deleteFile: DeleteFile} = await cloudProvider<{deleteFile: DeleteFile}>();

    return deleteFile(storageFile);
  }

  throw new Error('No provider to delete online file.');
};
