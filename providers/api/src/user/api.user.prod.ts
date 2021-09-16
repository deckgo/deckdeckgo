import {ApiUser, ApiUserInfo} from '@deckdeckgo/editor';

export const queryProd = ({
  apiUserInfo,
  token,
  context,
  method,
  apiUrl
}: {
  apiUserInfo: ApiUserInfo | ApiUser;
  token: string;
  context: string;
  method: string;
  apiUrl: string;
}): Promise<ApiUser | undefined> => {
  return new Promise<ApiUser | undefined>(async (resolve, reject) => {
    try {
      if (!apiUrl) {
        resolve(undefined);
        return;
      }

      const rawResponse: Response = await fetch(apiUrl + context, {
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

      resolve(persistedUser);
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteProd = ({userId, token, apiUrl}: {userId: string; token: string; apiUrl: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!apiUrl) {
        resolve();
        return;
      }

      const rawResponse: Response = await fetch(apiUrl + `/users/${userId}`, {
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
};

export const getProd = ({userId, apiUrl}: {userId: string; apiUrl: string}): Promise<ApiUser | undefined> => {
  return new Promise<ApiUser | undefined>(async (resolve, reject) => {
    try {
      if (!apiUrl) {
        resolve(undefined);
        return;
      }

      const rawResponse: Response = await fetch(apiUrl + `/users/${userId}`, {
        method: 'GET',
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json'
        }
      });

      if (!rawResponse || !rawResponse.ok) {
        // 404 if not found
        resolve(undefined);
        return;
      }

      const persistedUser: ApiUser = await rawResponse.json();

      resolve(persistedUser);
    } catch (err) {
      reject(err);
    }
  });
};
