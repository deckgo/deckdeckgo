import {ApiUser, ApiUserInfo} from '@deckdeckgo/editor';

/**
 * We don't have a staging API server currently, therefore we mock the calls.
 * @see https://github.com/deckgo/deckdeckgo/issues/274
 */

export const queryMock = ({apiUserInfo}: {apiUserInfo: ApiUserInfo | ApiUser}): Promise<ApiUser | undefined> => {
  return new Promise<ApiUser>(async (resolve) => {
    const testUser: ApiUser = createTestUserInfo(apiUserInfo);

    resolve(testUser);
  });
};

const createTestUserInfo = (apiUserInfo: ApiUserInfo | ApiUser): ApiUser => {
  return {
    id: apiUserInfo.firebase_uid,
    anonymous: false,
    firebase_uid: apiUserInfo.firebase_uid,
    username: 'Peter Parker'
  };
};

export const deleteMock = (): Promise<void> => {
  return Promise.resolve();
};

export const getMock = (): Promise<ApiUser | undefined> => {
  return Promise.resolve(undefined);
};
