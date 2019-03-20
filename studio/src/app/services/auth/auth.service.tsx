import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from 'firebase';

import {BehaviorSubject, Observable} from 'rxjs';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {User} from '../../models/user';

export class AuthService {

    private userSubject: BehaviorSubject<User> = new BehaviorSubject(null);

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
        return new Promise<void>((resolve) => {
            firebase.initializeApp(EnvironmentConfigService.getInstance().get('firebase'));

            firebase.auth().onAuthStateChanged(async (user: FirebaseUser) => {
                if (!user) {
                    this.userSubject.next(null);
                } else {
                    const tokenId: string = await user.getIdToken();

                    this.userSubject.next({
                        token: tokenId,
                        name: user.displayName,
                        email: user.email,
                        email_verified: user.emailVerified,
                        photo_url: user.photoURL
                    });
                }
            });

            resolve();
        });
    }

    async logout() {
        await firebase.auth().signOut();
    }

    watch(): Observable<User> {
        return this.userSubject.asObservable();
    }
}
