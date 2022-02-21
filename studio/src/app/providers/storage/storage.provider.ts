import {DeleteFile, GetFiles, StorageFile, StorageFilesList, UploadFile} from '@deckdeckgo/editor';
import {offlineStore} from '@deckdeckgo/studio';
import {authStore} from '@deckdeckgo/studio';
import {Constants} from '../../config/constants';
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
      userId: authStore.default.state.authUser.uid
    });
  }

  throw new Error('No provider to upload file online.');
};

export const getFiles = async ({next, folder}: {next: string | null; folder: string}): Promise<StorageFilesList | null> => {
  if (!authStore.default.state.loggedIn || !offlineStore.default.state.online) {
    return StorageOfflineProvider.getInstance().getFiles(folder);
  }

  if (cloud()) {
    const {getFiles}: {getFiles: GetFiles} = await cloudProvider<{getFiles: GetFiles}>();

    return getFiles({
      next,
      maxResults: Constants.STORAGE.MAX_QUERY_RESULTS,
      folder,
      userId: authStore.default.state.authUser.uid
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
