import {Observable, ReplaySubject} from 'rxjs';

import {ApiUser, ApiUserInfo} from '../../../models/api/api.user';
import {AuthUser} from '../../../models/auth/auth.user';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

export abstract class ApiUserService {

    protected apiUserSubject: ReplaySubject<ApiUser> = new ReplaySubject(1);

    abstract query(apiUserInfo: ApiUserInfo | ApiUser, token: string, context: string, method: string): Promise<ApiUser>;

    abstract delete(userId: string, token: string): Promise<void>;

    signIn(authUser: AuthUser): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!authUser) {
                await this.signOut();
            } else {
                // If anonymous do nothing
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
                    // We don't display the error. The user could continue to work and edit his/her presentations.
                    console.error(err);
                }
            }

            resolve();
        });
    }

    signOut(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.apiUserSubject.next(null);

            resolve();
        });
    }

    post(apiUser: ApiUserInfo, token: string): Promise<ApiUser> {
        return this.query(apiUser, token, '/users', 'POST');
    }

    put(apiUser: ApiUserInfo | ApiUser, token: string, userId: string): Promise<ApiUser> {
        return this.query(apiUser, token, `/users/${userId}`, 'PUT');
    }

    private get(userId: string): Promise<ApiUser> {
        return new Promise<ApiUser>(async (resolve, reject) => {
            try {
                const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

                const rawResponse: Response = await fetch(config.apiUrl + `/users/${userId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    // 404 if not found
                    resolve(null);
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

    watch(): Observable<ApiUser> {
        return this.apiUserSubject.asObservable();
    }

    private createUserInfo(authUser: AuthUser): Promise<ApiUserInfo> {
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
