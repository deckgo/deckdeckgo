import {ApiUser as ApiUserModel, ApiUserInfo, AuthUser} from '@deckdeckgo/editor';

import {deleteProd, getProd, queryProd} from './api.user.prod';
import {deleteMock, getMock, queryMock} from './api.user.mock';

export const signInApi = ({
  authUser,
  mock,
  apiUrl,
  token
}: {
  authUser: AuthUser;
  mock: boolean;
  apiUrl: string;
  token: string;
}): Promise<ApiUserModel | undefined> => {
  return new Promise<ApiUserModel | undefined>(async (resolve) => {
    if (!authUser || !authUser.uid) {
      resolve(undefined);
      return;
    }

    try {
      const user: ApiUserModel | undefined = await getApi({
        userId: authUser.uid,
        mock,
        apiUrl
      });

      if (!user) {
        const apiUser: ApiUserInfo | null = createUserInfo(authUser);

        const newUser: ApiUserModel | undefined = await postApi({
          apiUserInfo: apiUser,
          mock,
          apiUrl,
          token
        });

        resolve(newUser);
        return;
      }

      resolve(user);
    } catch (err) {
      // We don't display the error. The user could continue to work and edit his/her presentations.
      console.error(err);
      resolve(undefined);
    }
  });
};

export const postApi = ({
  apiUserInfo,
  token,
  mock,
  apiUrl
}: {
  apiUserInfo: ApiUserInfo;
  token: string;
  mock: boolean;
  apiUrl: string;
}): Promise<ApiUserModel | undefined> => {
  return queryApi({
    apiUserInfo,
    token,
    context: '/users',
    method: 'POST',
    mock,
    apiUrl
  });
};

export const putApi = ({
  apiUserInfo,
  token,
  userId,
  mock,
  apiUrl
}: {
  apiUserInfo: ApiUserInfo | ApiUserModel;
  token: string;
  userId: string;
  mock: boolean;
  apiUrl: string;
}): Promise<ApiUserModel | undefined> => {
  return queryApi({
    apiUserInfo,
    token,
    context: `/users/${userId}`,
    method: 'PUT',
    mock,
    apiUrl
  });
};

export const queryApi = ({
  apiUserInfo,
  token,
  context,
  method,
  mock,
  apiUrl
}: {
  apiUserInfo: ApiUserInfo | ApiUserModel;
  token: string;
  context: string;
  method: string;
  mock: boolean;
  apiUrl: string;
}): Promise<ApiUserModel | undefined> => {
  if (mock) {
    return queryMock({apiUserInfo});
  }

  return queryProd({apiUserInfo, token, context, method, apiUrl});
};

export const deleteApi = ({token, mock, apiUrl, userId}: {token: string; mock: boolean; apiUrl: string; userId: string}): Promise<void> => {
  if (mock) {
    return deleteMock();
  }

  return deleteProd({userId, token, apiUrl});
};

export const getApi = ({userId, mock, apiUrl}: {userId: string; mock: boolean; apiUrl: string}): Promise<ApiUserModel | undefined> => {
  if (mock) {
    return getMock();
  }

  return getProd({apiUrl, userId});
};

const createUserInfo = (authUser: AuthUser): ApiUserInfo => {
  return {
    anonymous: false,
    firebase_uid: authUser.uid,
    email: authUser.email
  } as ApiUserInfo;
};
