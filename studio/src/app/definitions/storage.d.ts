interface StorageFile {
    fullPath: string;
    name: string;
    downloadUrl: string;
}

interface StorageFilesList {
    items: StorageFile[];
    nextPageToken: string | null;
}
