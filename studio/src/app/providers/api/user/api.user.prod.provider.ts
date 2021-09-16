import {ApiUser, ApiUserInfo} from '@deckdeckgo/editor';

import apiUserStore from '../../../stores/api.user.store';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';

import {ApiUserProvider} from './api.user.provider';

export class ApiUserProdProvider extends ApiUserProvider {
  // @Override
  query(apiUserInfo: ApiUserInfo | ApiUser, token: string, context: string, method: string): Promise<ApiUser | undefined> {
    return new Promise<ApiUser | undefined>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        if (!config?.apiUrl) {
          resolve(undefined);
          return;
        }

        const rawResponse: Response = await fetch(config.apiUrl + context, {
          method: method,
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`
          },
          body: JSON.stringify(apiUserInfo)
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

        if (!config?.apiUrl) {
          resolve();
          return;
        }

        const rawResponse: Response = await fetch(config.apiUrl + `/users/${userId}`, {
          method: 'DELETE',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`
          }
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
  get(userId: string): Promise<ApiUser | undefined> {
    return new Promise<ApiUser | undefined>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        if (!config?.apiUrl) {
          resolve(undefined);
          return;
        }

        const rawResponse: Response = await fetch(config.apiUrl + `/users/${userId}`, {
          method: 'GET',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json'
          }
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
