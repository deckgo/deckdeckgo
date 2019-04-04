import {Observable, ReplaySubject} from 'rxjs';

import {get, del} from 'idb-keyval';

import {User} from '../../models/user';
import {ApiUser} from '../../models/api-user';

import {EnvironmentConfigService} from '../environment/environment-config.service';

export class ApiService {

    private userIdSubject: ReplaySubject<string> = new ReplaySubject(1);

    private static instance: ApiService;

    private constructor() {
        // Private constructor, singleton
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
                        await this.query(apiUser, 'POST');
                    } catch (err) {
                        // TODO: Catch error to find if use is existing? Alternatively var isNewUser = authResult.additionalUserInfo.isNewUser;?
                    }

                } else {
                    // TODO: I need a get to load the object and not just play with the uid (for example in the future when user will upload their profile image)

                    this.userIdSubject.next(savedApiUserId);
                }
            }

            resolve();
        });
    }

    private query(apiUser: ApiUser, method: string): Promise<string> {
        return new Promise<string>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + '/users', {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(apiUser)
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating a user');
                    return;
                }

                console.log(rawResponse);

                // TODO What type I got back?
                // const persistedDeck: Deck = await rawResponse.json();

                // TODO userIdSubject.next

                // TODO set cookie

                resolve(null);
            } catch (err) {
                reject(err);
            }
        });
    }

    watch(): Observable<string> {
        return this.userIdSubject.asObservable();
    }
}
