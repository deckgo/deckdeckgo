import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from 'firebase';

import {Observable, ReplaySubject, Subject} from 'rxjs';

import {get, set, del} from 'idb-keyval';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {User} from '../../models/user';

import {ErrorService} from '../error/error.service';

export enum SignInType {
    SIGNIN,
    SIGNIN_MERGE_ANONYMOUS
}

export class AuthService {

    private userSubject: ReplaySubject<User> = new ReplaySubject(1);
    private signInSubject: Subject<SignInType> = new Subject();

    private errorService: ErrorService;

    private static instance: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
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
                        token: tokenId,
                        anonymous: authUser.isAnonymous,
                        name: authUser.displayName,
                        email: authUser.email,
                        email_verified: authUser.emailVerified,
                        photo_url: authUser.photoURL
                    };

                    this.userSubject.next(user);
                    await set('deckdeckgo_user', user);
                }
            });

            resolve();
        });
    }

    async logout() {
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

    watchSignInNavigation(): Observable<SignInType> {
        return this.signInSubject.asObservable();
    }

    navigateSignIn(componentProps: SignInType) {
        this.signInSubject.next(componentProps);
    }
}
