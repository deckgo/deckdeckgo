import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from 'firebase';

import {BehaviorSubject, Observable, ReplaySubject} from 'rxjs';

import {get, set, del} from 'idb-keyval';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {User} from '../../models/user';

export interface LoginModalComponentProps {
    anonymous: boolean,
    context?: string,
    onPresent?: Function
}

export class AuthService {

    private userSubject: ReplaySubject<User> = new ReplaySubject(1);
    private modalSubject: BehaviorSubject<LoginModalComponentProps> = new BehaviorSubject(null);

    private static instance: AuthService;

    private constructor() {
        // Private constructor, singleton
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

    watch(): Observable<User> {
        return this.userSubject.asObservable();
    }

    watchModal(): Observable<LoginModalComponentProps> {
        return this.modalSubject.asObservable();
    }

    openSignInModal(componentProps: LoginModalComponentProps) {
        this.modalSubject.next(componentProps);
    }
}
