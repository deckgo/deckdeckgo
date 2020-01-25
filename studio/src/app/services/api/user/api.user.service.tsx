import {Observable, ReplaySubject} from 'rxjs';

import {ApiUser, ApiUserInfo} from '../../../models/api/api.user';
import {AuthUser} from '../../../models/auth/auth.user';

export abstract class ApiUserService {
  protected apiUserSubject: ReplaySubject<ApiUser> = new ReplaySubject(1);

  abstract query(apiUserInfo: ApiUserInfo | ApiUser, token: string, context: string, method: string): Promise<ApiUser>;

  abstract delete(userId: string, token: string): Promise<void>;

  abstract get(userId: string): Promise<ApiUser>;

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
