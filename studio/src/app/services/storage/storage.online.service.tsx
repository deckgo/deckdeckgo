import {firebase} from '@firebase/app';
import '@firebase/storage';
import {Reference} from '@firebase/storage-types';

import {take} from 'rxjs/operators';

import {AuthUser} from '../../models/auth/auth.user';
import {AuthService} from '../auth/auth.service';
import {ErrorService} from '../core/error/error.service';

export class StorageOnlineService {
  private static instance: StorageOnlineService;

  private authService: AuthService;
  private errorService: ErrorService;

  private constructor() {
    this.errorService = ErrorService.getInstance();
    this.authService = AuthService.getInstance();
  }

  static getInstance() {
    if (!StorageOnlineService.instance) {
      StorageOnlineService.instance = new StorageOnlineService();
    }
    return StorageOnlineService.instance;
  }

  uploadFile(data: File, folder: string, maxSize: number): Promise<StorageFile> {
    return new Promise<StorageFile>((resolve) => {
      try {
        this.authService
          .watch()
          .pipe(take(1))
          .subscribe(async (authUser: AuthUser) => {
            if (!authUser || !authUser.uid || authUser.uid === '' || authUser.uid === undefined) {
              this.errorService.error('Not logged in.');
              resolve();
              return;
            }

            if (!data || !data.name) {
              this.errorService.error('File not valid.');
              resolve();
              return;
            }

            if (data.size > maxSize) {
              this.errorService.error(`File is too big (max. ${maxSize / 1048576} Mb)`);
              resolve();
              return;
            }

            const ref: Reference = firebase.storage().ref(`${authUser.uid}/assets/${folder}/${data.name}`);

            await ref.put(data);

            resolve({
              downloadUrl: await ref.getDownloadURL(),
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
}
