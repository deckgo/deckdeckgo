import {Observable, ReplaySubject} from 'rxjs';
import {take} from 'rxjs/operators';

import {get, del, set} from 'idb-keyval';

import {AuthUser} from '../../../models/auth-user';
import {User, UserInfo} from '../../../models/user';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {ErrorService} from '../../core/error/error.service';

export class ApiUserService {

    private apiUserSubject: ReplaySubject<User> = new ReplaySubject(1);

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

    authStateChanged(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!authUser) {
                await this.signOut();
            } else {
                const savedApiUserId: string = await get<string>('deckdeckgo_user_id');
                if (!savedApiUserId || savedApiUserId !== authUser.uid) {
                    const apiUser: UserInfo = await this.createUserInfo(authUser);

                    try {
                        await this.post(apiUser, authUser.token);
                    } catch (err) {
                        this.errorService.error(err);
                    }
                } else {
                    try {
                        await this.get(savedApiUserId);

                        await this.updateMergedUser(authUser);
                    } catch (err) {
                        this.errorService.error(err);
                    }
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

    post(apiUser: UserInfo, token: string): Promise<User> {
        return this.query(apiUser, token, '/users', 'POST');
    }

    put(apiUser: UserInfo | User, token: string, userId: string): Promise<User> {
        return this.query(apiUser, token, `/users/${userId}`, 'PUT');
    }

    query(apiUserInfo: UserInfo | User, token: string, context: string, method: string): Promise<User> {
        return new Promise<User>(async (resolve, reject) => {
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

                const persistedUser: User = await rawResponse.json();

                this.apiUserSubject.next(persistedUser);

                await set('deckdeckgo_user_id', persistedUser.id);

                resolve(persistedUser);
            } catch (err) {
                reject(err);
            }
        });
    }

    private get(userId: string): Promise<User> {
        return new Promise<User>(async (resolve, reject) => {
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

                const persistedUser: User = await rawResponse.json();

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

    watch(): Observable<User> {
        return this.apiUserSubject.asObservable();
    }

    createUserInfo(authUser: AuthUser): Promise<UserInfo> {
        return new Promise<UserInfo>((resolve) => {
            if (!authUser) {
                resolve(null);
                return;
            }

            const apiUserInfo: UserInfo = {
                anonymous: authUser.anonymous,
                firebase_uid: authUser.uid,
                email: authUser.anonymous ? null : authUser.email
            };

            resolve(apiUserInfo);
        });
    }

    // In case of successful merge, previous user who might have been an anonymous need to become a not anonymous one and get a user name
    private updateMergedUser(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            if (!authUser) {
                resolve();
                return;
            }

            const mergeInfo: MergeInformation = await get<MergeInformation>('deckdeckgo_redirect_info');

            if (!authUser || authUser.anonymous || !mergeInfo || !mergeInfo.anonymous || !mergeInfo.userId || !mergeInfo.userToken) {
                resolve();
                return;
            }

            this.watch().pipe(take(1)).subscribe(async (user: User) => {
                if (!user || user.username) {
                    resolve();
                    return;
                }

                try {
                    const apiUser: UserInfo = await this.createUserInfo(authUser);
                    await this.put(apiUser, authUser.token, user.id);

                    resolve();
                } catch (err) {
                    reject(err);
                }
            });
        })
    }

}
