export interface StorageFile {
  fullPath: string;
  name: string;
  downloadUrl: string | undefined;
}

export interface StorageFilesList {
  items: StorageFile[];
  nextPageToken: string | null;
}

export interface StorageFolder {
  name: string;
}

export interface StorageFoldersList {
  prefixes: StorageFolder[];
}
