interface StorageFile {
    downloadUrl: string;
}

interface StorageFilesList {
    items: StorageFile[];
    nextPageToken: string | null;
}
