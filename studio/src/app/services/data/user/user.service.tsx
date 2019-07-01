import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {Observable, ReplaySubject} from 'rxjs';

import {AuthUser} from '../../../models/auth/auth.user';
import {User, UserData} from '../../../models/data/user';

export class UserService {

    private userSubject: ReplaySubject<User> = new ReplaySubject(1);

    private static instance: UserService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!UserService.instance) {
            UserService.instance = new UserService();
        }
        return UserService.instance;
    }

    create(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            if (!authUser || !authUser.uid) {
                reject('Authentication user not defined.');
                return;
            }

            try {
                const firestore: firebase.firestore.Firestore = firebase.firestore();

                const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection('users').doc(authUser.uid).get();

                if (!snapshot.exists) {
                    const user: User = await this.createUser(authUser);

                    this.userSubject.next(user);
                } else {
                    const user: UserData = snapshot.data() as UserData;

                    const updatedUser: UserData = await this.updateUser(authUser, user);

                    this.userSubject.next({
                        id: authUser.uid,
                        data: updatedUser
                    });
                }

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    private createUser(authUser: AuthUser): Promise<User> {
        return new Promise<User>(async (resolve, reject) => {
            try {
                const firestore: firebase.firestore.Firestore = firebase.firestore();

                const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

                const user: UserData = {
                    anonymous: authUser.anonymous,
                    created_at: now,
                    updated_at: now
                };

                if (authUser.name) {
                    user.name = authUser.name;
                }

                if (authUser.photo_url) {
                    user.photo_url = authUser.photo_url;
                }

                await firestore.collection('users').doc(authUser.uid).set(user, {merge: true});

                resolve({
                    id: authUser.uid,
                    data: user
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateUser(authUser: AuthUser, user: UserData): Promise<UserData> {
        return new Promise<UserData>(async (resolve, reject) => {
            try {
                if (this.userNeedUpdate(authUser, user)) {
                    const firestore: firebase.firestore.Firestore = firebase.firestore();

                    if (user.anonymous !== authUser.anonymous) {
                        user.anonymous = authUser.anonymous;
                    }

                    if ((!user.name && authUser.name)) {
                        user.name = authUser.name;
                    }

                    if ((!user.photo_url && authUser.photo_url)) {
                        user.photo_url = authUser.photo_url;
                    }

                    user.updated_at = firebase.firestore.Timestamp.now();

                    await firestore.collection('users').doc(authUser.uid).set(user, {merge: true});
                }

                resolve(user);
            } catch (err) {
                reject(err);
            }
        });
    }

    private userNeedUpdate(authUser: AuthUser, user: UserData): boolean {
        if (user.anonymous !== authUser.anonymous) {
            return true;
        } else if (!user.name && authUser.name) {
            return true;
        } else if (!user.photo_url && authUser.photo_url) {
            return true;
        } else {
            return false;
        }
    }

    watch(): Observable<User> {
        return this.userSubject.asObservable();
    }

    delete(userId: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const firestore: firebase.firestore.Firestore = firebase.firestore();

                await firestore.collection('users').doc(userId).delete();

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }
}
