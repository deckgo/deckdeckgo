import {Observable, ReplaySubject} from 'rxjs';

import {get, del, set} from 'idb-keyval';

import {User} from '../../models/user';
import {ApiUser} from '../../models/api-user';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {ErrorService} from '../error/error.service';

export class ApiService {

    private userIdSubject: ReplaySubject<string> = new ReplaySubject(1);

    private static instance: ApiService;

    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!ApiService.instance) {
            ApiService.instance = new ApiService();
        }
        return ApiService.instance;
    }

    // TODO: Implement the authentication in each API requests which need it

    onAuthStateChanged(user: User): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!user) {
                this.userIdSubject.next(null);
                await del('deckdeckgo_api_user');
            } else {
                const savedApiUserId: string = await get('deckdeckgo_api_user');
                if (!savedApiUserId) {
                    const apiUser: ApiUser = {
                        user_anonymous: user.anonymous,
                        user_firebase_uid: user.uid
                    };

                    try {
                        await this.query(apiUser, user.token, 'POST');
                    } catch (err) {
                        // TODO: Catch error to find if use is existing? Alternatively var isNewUser = authResult.additionalUserInfo.isNewUser;?
                    }

                } else {
                    try {
                        await this.get(savedApiUserId);
                    } catch (err) {
                        this.errorService.error(err);
                    }

                    this.userIdSubject.next(savedApiUserId);
                }
            }

            resolve();
        });
    }

    private query(apiUser: ApiUser, token: string, method: string): Promise<ApiUser> {
        return new Promise<ApiUser>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + '/users', {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    },
                    body: JSON.stringify(apiUser)
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating a user');
                    return;
                }

                const persistedUser: ApiUser = await rawResponse.json();

                // TODO spread object not id
                this.userIdSubject.next(persistedUser.user_id);

                await set('deckdeckgo_api_user', persistedUser.user_id);

                resolve(persistedUser);
            } catch (err) {
                reject(err);
            }
        });
    }

    private get(userId: string): Promise<ApiUser> {
        return new Promise<ApiUser>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/users/${userId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating a user');
                    return;
                }

                const persistedUser: ApiUser = await rawResponse.json();

                this.userIdSubject.next(persistedUser.user_id);

                resolve(persistedUser);
            } catch (err) {
                reject(err);
            }
        });
    }

    watch(): Observable<string> {
        return this.userIdSubject.asObservable();
    }
}
