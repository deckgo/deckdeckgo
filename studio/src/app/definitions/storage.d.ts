interface StorageFile {
    fullPath: string;
    name: string;
    fullUrl?: string;
}

interface StorageFolder extends StorageFile {
    folder: boolean;
    displayName: string;
}

interface StorageFilesList {
    items: StorageFile[];
    nextPageToken: string | null;
}

interface StorageUploadFolderMetaInfo {
    deckName: string;
}

interface StorageUploadInfo {
    maxSize: number;
    privateFile: boolean;
    folder: string;
    folderMeta?: StorageUploadFolderMetaInfo;
}
