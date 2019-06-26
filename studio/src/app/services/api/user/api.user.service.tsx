import {Observable, ReplaySubject} from 'rxjs';

import {del, set} from 'idb-keyval';

import {AuthUser} from '../../../models/auth/auth.user';
import {ApiUser, ApiUserInfo} from '../../../models/api/api.user';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {ErrorService} from '../../core/error/error.service';

export class ApiUserService {

    private apiUserSubject: ReplaySubject<ApiUser> = new ReplaySubject(1);

    private static instance: ApiUserService;

    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!ApiUserService.instance) {
            ApiUserService.instance = new ApiUserService();
        }
        return ApiUserService.instance;
    }

    signIn(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!authUser) {
                await this.signOut();
            } else {
                // Uf an
                if (authUser.anonymous) {
                    resolve();
                    return;
                }

                try {
                    const user: ApiUser = await this.get(authUser.uid);

                    if (!user) {
                        const apiUser: ApiUserInfo = await this.createUserInfo(authUser);

                        await this.post(apiUser, authUser.token);
                    }
                } catch (err) {
                    this.errorService.error(err);
                }
            }

            resolve();
        });
    }

    signOut(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.apiUserSubject.next(null);

            await del('deckdeckgo_user_id');

            resolve();
        });
    }

    post(apiUser: ApiUserInfo, token: string): Promise<ApiUser> {
        return this.query(apiUser, token, '/users', 'POST');
    }

    put(apiUser: ApiUserInfo | ApiUser, token: string, userId: string): Promise<ApiUser> {
        return this.query(apiUser, token, `/users/${userId}`, 'PUT');
    }

    query(apiUserInfo: ApiUserInfo | ApiUser, token: string, context: string, method: string): Promise<ApiUser> {
        return new Promise<ApiUser>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    },
                    body: JSON.stringify(apiUserInfo)
                });

                if (!rawResponse || (!rawResponse.ok && rawResponse.status !== 409)) {
                    reject('Something went wrong while creating a user');
                    return;
                }

                const persistedUser: ApiUser = await rawResponse.json();

                this.apiUserSubject.next(persistedUser);

                await set('deckdeckgo_user_id', persistedUser.id);

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

                this.apiUserSubject.next(persistedUser);

                resolve(persistedUser);
            } catch (err) {
                reject(err);
            }
        });
    }

    delete(userId: string, token: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/users/${userId}`, {
                    method: 'DELETE',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': `Bearer ${token}`
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating a user');
                    return;
                }

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    watch(): Observable<ApiUser> {
        return this.apiUserSubject.asObservable();
    }

    createUserInfo(authUser: AuthUser): Promise<ApiUserInfo> {
        return new Promise<ApiUserInfo>((resolve) => {
            if (!authUser) {
                resolve(null);
                return;
            }

            const apiUserInfo: ApiUserInfo = {
                anonymous: authUser.anonymous,
                firebase_uid: authUser.uid,
                email: authUser.anonymous ? null : authUser.email
            };

            resolve(apiUserInfo);
        });
    }

}
