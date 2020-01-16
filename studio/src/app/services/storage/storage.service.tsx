import {firebase} from '@firebase/app';
import '@firebase/storage';
import {Reference, ListResult, ListOptions} from '@firebase/storage-types';

import {take} from 'rxjs/operators';

import {AuthUser} from '../../models/auth/auth.user';

import {Resources} from '../../utils/core/resources';

import {ErrorService} from '../core/error/error.service';
import {AuthService} from '../auth/auth.service';
import {EnvironmentConfigService} from '../core/environment/environment-config.service';

export class StorageService {

    private static instance: StorageService;

    private authService: AuthService;
    private errorService: ErrorService;

    maxQueryResults: number = 20;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!StorageService.instance) {
            StorageService.instance = new StorageService();
        }
        return StorageService.instance;
    }

    uploadFile(data: File, info: StorageUploadInfo): Promise<StorageFile> {
        return new Promise<StorageFile>((resolve) => {
            try {
                this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                    if (!authUser || !authUser.uid || authUser.uid === '' || authUser.uid === undefined) {
                        this.errorService.error('Not logged in.');
                        resolve();
                        return;
                    }

                    if (!info) {
                        this.errorService.error('No upload information provided.');
                        resolve();
                        return;
                    }

                    if (!data || !data.name) {
                        this.errorService.error('Image not valid.');
                        resolve();
                        return;
                    }

                    if (data.size > 10485760) {
                        this.errorService.error(`Image is too big (max. ${info.maxSize / 1048576} Mb)`);
                        resolve();
                        return;
                    }

                    const ref: Reference = firebase.storage().ref(`${authUser.uid}/assets/${info.folder}/${data.name}`);

                    const metaData: firebase.storage.UploadMetadata = info.privateFile ? {customMetadata: {public: 'false'}} : undefined;

                    await ref.put(data, metaData);

                    await this.uploadFolderMetadata(authUser, info);

                    const fullUrl: string = await this.getFileUrl(ref);

                    resolve({
                        fullUrl: fullUrl,
                        fullPath: ref.fullPath,
                        name: ref.name
                    });
                });
            } catch (err) {
                this.errorService.error(err.message);
                resolve();
            }
        });
    }

    private uploadFolderMetadata(authUser: AuthUser, info: StorageUploadInfo): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!info || !info.folderMeta || info.folderMeta === undefined) {
                    resolve();
                    return;
                }

                const ref: Reference = firebase.storage().ref(`${authUser.uid}/assets/${info.folder}/${Resources.Constants.STORAGE.FOLDER.META_FILENAME}`);

                const blob: Blob = new Blob([JSON.stringify(info.folderMeta)], {type: 'application/json'});

                await ref.put(blob, {
                    customMetadata: {
                        public: 'false',
                        deckName: info.folderMeta.deckName
                    }
                });

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    getFiles(next: string | null, folder: string): Promise<StorageFilesList | null> {
        return new Promise<StorageFilesList | null>((resolve) => {
            try {
                this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                    if (!authUser || !authUser.uid || authUser.uid === '' || authUser.uid === undefined) {
                        this.errorService.error('Not logged in.');
                        resolve(null);
                        return;
                    }

                    const ref = firebase.storage().ref(`${authUser.uid}/assets/${folder}/`);

                    let options: ListOptions = {
                        maxResults: this.maxQueryResults
                    };

                    if (next) {
                        options.pageToken = next
                    }

                    const results: ListResult = await ref.list(options);

                    resolve(this.toStorageFileList(results));
                });
            } catch (err) {
                resolve(null);
            }
        })
    }

    private toStorageFileList(results: ListResult): Promise<StorageFilesList> {
        return new Promise<StorageFilesList>(async (resolve) => {
            if (!results ||
                ((!results.items || results.items.length <= 0) && ((!results.prefixes || results.prefixes.length <= 0)))) {
                resolve({
                    items: [],
                    nextPageToken: null
                });
                return;
            }

            const folders: StorageFolder[] | undefined = await this.toStorageFolderItems(results.prefixes);

            const files: StorageFile[] | undefined = await this.toStorageFilesItems(results.items);

            let items: StorageFile[] = [];

            if (folders && folders.length > 0) {
                items = [...folders];
            }

            if (files && files.length > 0) {
                items = [...items, ...files];
            }

            resolve({
                items: items,
                nextPageToken: results.nextPageToken
            });
        });
    }

    private toStorageFolderItems(prefixes: Reference[]): Promise<StorageFolder[] | undefined> {
        return new Promise<StorageFolder[] | undefined>(async (resolve) => {
            if (!prefixes || prefixes.length <= 0) {
                resolve(undefined);
                return;
            }

            const storageFolders: Promise<StorageFolder>[] = prefixes.map((prefix: Reference) => {
                return this.toStorageFolder(prefix);
            });

            const items: StorageFolder[] = await Promise.all(storageFolders);

            resolve(items);
        });
    }

    private toStorageFilesItems(storageFilesRefs: Reference[]): Promise<StorageFile[] | undefined> {
        return new Promise<StorageFile[] | undefined>(async (resolve) => {
            const storageFiles: Promise<StorageFile>[] = storageFilesRefs.filter((ref: Reference) => {
                return ref.name !== Resources.Constants.STORAGE.FOLDER.META_FILENAME;
            }).map((ref: Reference) => {
                return this.toStorageFile(ref, this);
            });
            const items: StorageFile[] = await Promise.all(storageFiles);

            resolve(items);
        });
    }

    private toStorageFile(ref: Reference, self): Promise<StorageFile> {
        return new Promise<StorageFile>(async (resolve) => {
            const fullUrl: string = await self.getFileUrl(ref);

            resolve({
                fullUrl: fullUrl,
                fullPath: ref.fullPath,
                name: ref.name
            });
        });
    }

    private toStorageFolder(ref: Reference): Promise<StorageFolder> {
        return new Promise<StorageFolder>(async (resolve) => {
            const refMeta = firebase.storage().ref(`${ref.fullPath}/${Resources.Constants.STORAGE.FOLDER.META_FILENAME}`);
            const metaData = await refMeta.getMetadata();

            resolve({
                fullPath: ref.fullPath,
                name: ref.name,
                folder: true,
                displayName: metaData && metaData.customMetadata && metaData.customMetadata.deckName ? metaData.customMetadata.deckName : 'Folder'
            });
        });
    }

    private getFileUrl(ref: Reference): Promise<string> {
        return new Promise<string>((resolve) => {
            const storageUrl: string = EnvironmentConfigService.getInstance().get('firebase').storageUrl;

            resolve(storageUrl + encodeURIComponent(ref.fullPath) + `?${Resources.Constants.STORAGE.MEDIA_PARAM}`);
        });
    }
}
