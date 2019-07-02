interface StorageFile {
    fullPath: string;
    downloadUrl: string;
}

interface StorageFilesList {
    items: StorageFile[];
    nextPageToken: string | null;
}
