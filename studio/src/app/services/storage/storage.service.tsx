import {firebase} from '@firebase/app';
import '@firebase/storage';

import {take} from 'rxjs/operators';

import {ApiUser} from '../../models/api/api.user';

import {ErrorService} from '../core/error/error.service';
import {ApiUserService} from '../api/user/api.user.service';

export class StorageService {

    private static instance: StorageService;

    private apiUserService: ApiUserService;
    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
        this.apiUserService = ApiUserService.getInstance();
    }

    static getInstance() {
        if (!StorageService.instance) {
            StorageService.instance = new StorageService();
        }
        return StorageService.instance;
    }

    uploadImage(image: File): Promise<void> {
        return new Promise<void>((resolve) => {
            try {
                this.apiUserService.watch().pipe(take(1)).subscribe(async (apiUser: ApiUser) => {
                    if (!apiUser || !apiUser.username || apiUser.username === '' || apiUser.username === undefined) {
                        this.errorService.error('Not logged in.');
                        resolve();
                        return;
                    }

                    if (!image || !image.name) {
                        this.errorService.error('Image not valid.');
                        resolve();
                        return;
                    }

                    if (image.size > 2097152) {
                        this.errorService.error('Image is too big (max. 2 Mb)');
                        resolve();
                        return;
                    }

                    const ref = firebase.storage().ref(`${apiUser.username}/assets/images/${image.name}`);

                    await ref.put(image);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err.message);
                resolve();
            }
        });
    }
}
