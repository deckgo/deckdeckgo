import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from 'firebase';

import {Observable, ReplaySubject} from 'rxjs';
import {take} from 'rxjs/operators';

import {get, set, del} from 'idb-keyval';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {User} from '../../models/user';

import {ErrorService} from '../error/error.service';
import {ApiService} from '../api/api.service';

export class AuthService {

    private userSubject: ReplaySubject<User> = new ReplaySubject(1);

    private errorService: ErrorService;

    private apiService: ApiService;

    private static instance: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
        this.apiService = ApiService.getInstance();
    }

    static getInstance() {
        if (!AuthService.instance) {
            AuthService.instance = new AuthService();
        }
        return AuthService.instance;
    }

    init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            // We also save the user in the local storage to avoid a flickering in the GUI till Firebase as correctly fetched the user
            const localUser: User = await get('deckdeckgo_user');
            this.userSubject.next(localUser);

            firebase.initializeApp(EnvironmentConfigService.getInstance().get('firebase'));

            firebase.auth().onAuthStateChanged(async (authUser: FirebaseUser) => {
                if (!authUser) {
                    this.userSubject.next(null);
                    await del('deckdeckgo_user');
                } else {
                    const tokenId: string = await authUser.getIdToken();

                    const user: User = {
                        uid: authUser.uid,
                        token: tokenId,
                        anonymous: authUser.isAnonymous,
                        name: authUser.displayName,
                        email: authUser.email,
                        email_verified: authUser.emailVerified,
                        photo_url: authUser.photoURL
                    };

                    // Update anonymous user
                    // Reference: https://github.com/firebase/firebaseui-web/issues/449
                    if (!user.name && authUser.providerData && authUser.providerData.length > 0 && authUser.providerData[0].displayName) {
                        user.name = authUser.providerData[0].displayName;
                    }

                    if (!user.photo_url && authUser.providerData && authUser.providerData.length > 0 && authUser.providerData[0].photoURL) {
                        user.photo_url = authUser.providerData[0].photoURL;
                    }

                    await this.apiService.onAuthStateChanged(user);

                    this.userSubject.next(user);
                    await set('deckdeckgo_user', user);
                }
            });

            resolve();
        });
    }

    async signOut() {
        await del('deckdeckgo_user');
        await firebase.auth().signOut();
    }

    signInAnonymous(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                await firebase.auth().signInAnonymously();

                resolve();
            } catch (err) {
                this.errorService.error(err.message);
                resolve(err);
            }
        });
    }

    watch(): Observable<User> {
        return this.userSubject.asObservable();
    }

    getBearer(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.watch().pipe(take(1)).subscribe((user: User) => {
                resolve(`Bearer ${user ? user.token : ''}`)
            });
        });
    }
}
