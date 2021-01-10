import apiUserStore from '../../../stores/api.user.store';

import {ApiUser, ApiUserInfo} from '../../../models/api/api.user';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';

import {ApiUserService} from './api.user.service';

export class ApiUserProdService extends ApiUserService {
  // @Override
  query(apiUserInfo: ApiUserInfo | ApiUser, token: string, context: string, method: string): Promise<ApiUser> {
    return new Promise<ApiUser>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        const rawResponse: Response = await fetch(config.apiUrl + context, {
          method: method,
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`,
          },
          body: JSON.stringify(apiUserInfo),
        });

        if (!rawResponse || (!rawResponse.ok && rawResponse.status !== 409)) {
          reject('Something went wrong while creating a user');
          return;
        }

        const persistedUser: ApiUser = await rawResponse.json();

        apiUserStore.state.apiUser = {...persistedUser};

        resolve(persistedUser);
      } catch (err) {
        reject(err);
      }
    });
  }

  // @Override
  delete(userId: string, token: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        const rawResponse: Response = await fetch(config.apiUrl + `/users/${userId}`, {
          method: 'DELETE',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`,
          },
        });

        if (!rawResponse || !rawResponse.ok) {
          reject('Something went wrong while creating a user');
          return;
        }

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  // @Override
  get(userId: string): Promise<ApiUser> {
    return new Promise<ApiUser>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        const rawResponse: Response = await fetch(config.apiUrl + `/users/${userId}`, {
          method: 'GET',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
          },
        });

        if (!rawResponse || !rawResponse.ok) {
          // 404 if not found
          resolve(null);
          return;
        }

        const persistedUser: ApiUser = await rawResponse.json();

        apiUserStore.state.apiUser = {...persistedUser};

        resolve(persistedUser);
      } catch (err) {
        reject(err);
      }
    });
  }
}
