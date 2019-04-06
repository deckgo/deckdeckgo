import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from 'firebase';

import {Observable, ReplaySubject} from 'rxjs';
import {take} from 'rxjs/operators';

import {get, set, del} from 'idb-keyval';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {AuthUser} from '../../models/auth-user';

import {ErrorService} from '../error/error.service';
import {UserService} from '../user/user.service';

export class AuthService {

    private authUserSubject: ReplaySubject<AuthUser> = new ReplaySubject(1);

    private errorService: ErrorService;

    private userService: UserService;

    private static instance: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
        this.userService = UserService.getInstance();
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
            const localUser: AuthUser = await get('deckdeckgo_auth_user');
            this.authUserSubject.next(localUser);

            firebase.initializeApp(EnvironmentConfigService.getInstance().get('firebase'));

            firebase.auth().onAuthStateChanged(async (firebaseUser: FirebaseUser) => {

                console.log('Firebase user');

                if (!firebaseUser) {
                    this.authUserSubject.next(null);
                    await del('deckdeckgo_auth_user');
                } else {
                    const tokenId: string = await firebaseUser.getIdToken();

                    const authUser: AuthUser = {
                        uid: firebaseUser.uid,
                        token: tokenId,
                        anonymous: firebaseUser.isAnonymous,
                        name: firebaseUser.displayName,
                        email: firebaseUser.email,
                        email_verified: firebaseUser.emailVerified,
                        photo_url: firebaseUser.photoURL
                    };

                    // Update anonymous user
                    // Reference: https://github.com/firebase/firebaseui-web/issues/449
                    if (!authUser.name && firebaseUser.providerData && firebaseUser.providerData.length > 0 && firebaseUser.providerData[0].displayName) {
                        authUser.name = firebaseUser.providerData[0].displayName;
                    }

                    if (!authUser.photo_url && firebaseUser.providerData && firebaseUser.providerData.length > 0 && firebaseUser.providerData[0].photoURL) {
                        authUser.photo_url = firebaseUser.providerData[0].photoURL;
                    }

                    await set('deckdeckgo_auth_user', authUser);
                    this.authUserSubject.next(authUser);

                    await this.userService.authStateChanged(authUser);
                }
            });

            resolve();
        });
    }

    async signOut() {
        await del('deckdeckgo_auth_user');
        await firebase.auth().signOut();

        await this.userService.signOut();
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

    watch(): Observable<AuthUser> {
        return this.authUserSubject.asObservable();
    }

    getBearer(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.watch().pipe(take(1)).subscribe((authUser: AuthUser) => {
                resolve(`Bearer ${authUser ? authUser.token : ''}`)
            });
        });
    }
}
