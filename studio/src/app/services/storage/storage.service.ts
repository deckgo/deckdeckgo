export abstract class StorageService {
  abstract uploadFile(data: File, folder: string, maxSize: number, downloadUrl?: boolean): Promise<StorageFile | undefined>;

  abstract getFiles(next: string | null, folder: string): Promise<StorageFilesList | null>;

  abstract getFolders(folder: string): Promise<StorageFoldersList | undefined>;
}
