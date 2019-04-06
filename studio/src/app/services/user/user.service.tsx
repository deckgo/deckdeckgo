import {Observable, ReplaySubject} from 'rxjs';

import {get, del, set} from 'idb-keyval';

import {AuthUser} from '../../models/auth-user';
import {User} from '../../models/user';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {ErrorService} from '../error/error.service';

export class UserService {

    private apiUserSubject: ReplaySubject<User> = new ReplaySubject(1);

    private static instance: UserService;

    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!UserService.instance) {
            UserService.instance = new UserService();
        }
        return UserService.instance;
    }

    // TODO: Implement the authentication in each API requests which need it

    authStateChanged(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!authUser) {
                this.apiUserSubject.next(null);
                await del('deckdeckgo_user_id');
            } else {
                const savedApiUserId: string = await get('deckdeckgo_user_id');
                if (!savedApiUserId) {
                    const apiUser: User = {
                        user_anonymous: authUser.anonymous,
                        user_firebase_uid: authUser.uid
                    };

                    try {
                        await this.query(apiUser, authUser.token, 'POST');
                    } catch (err) {
                        // TODO: Catch error to find if use is existing? Alternatively var isNewUser = authResult.additionalUserInfo.isNewUser;?
                    }

                } else {
                    try {
                        await this.get(savedApiUserId);
                    } catch (err) {
                        this.errorService.error(err);
                    }
                }

                // TODO si firebase_uid !== previous_firebaseuid sauvegarder dans les cookies
                // updater deck.user_id avec nouvel id en passant token du previous authUser
            }

            resolve();
        });
    }

    private query(apiUser: User, token: string, method: string): Promise<User> {
        return new Promise<User>(async (resolve, reject) => {
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

    watch(): Observable<User> {
        return this.apiUserSubject.asObservable();
    }

}
