import apiUserStore from '../../../stores/api.user.store';

import {ApiUser, ApiUserInfo} from '../../../models/api/api.user';

import {ApiUserService} from './api.user.service';

/**
 * We don't have a staging API server currently, therefore we mock the calls.
 * @see https://github.com/deckgo/deckdeckgo/issues/274
 */
export class ApiUserMockService extends ApiUserService {
  // @Override
  query(apiUserInfo: ApiUserInfo | ApiUser, _token: string, _context: string, _method: string): Promise<ApiUser> {
    return new Promise<ApiUser>(async (resolve) => {
      const testUser: ApiUser = await this.createTestUserInfo(apiUserInfo);

      apiUserStore.state.apiUser = {...testUser};

      resolve(testUser);
    });
  }

  // @Override
  delete(_userId: string, _token: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      resolve();
    });
  }

  // @Override
  get(_userId: string): Promise<ApiUser> {
    return new Promise<ApiUser>(async (resolve) => {
      resolve(undefined);
    });
  }

  private createTestUserInfo(apiUserInfo: ApiUserInfo | ApiUser): Promise<ApiUser> {
    return new Promise<ApiUser>((resolve) => {
      const testUser: ApiUser = {
        id: apiUserInfo.firebase_uid,
        anonymous: false,
        firebase_uid: apiUserInfo.firebase_uid,
        username: 'Peter Parker',
      };

      resolve(testUser);
    });
  }
}
