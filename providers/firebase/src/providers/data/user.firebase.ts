import firebase from 'firebase/app';
import 'firebase/firestore';

import {AuthUser, CreateUser, DeleteUser, UpdateUser, User, UserData} from '@deckdeckgo/editor';
import {ApiUserData} from '../../types/api.user';

export const createUser: CreateUser = (authUser: AuthUser): Promise<User> => {
  return new Promise<User>(async (resolve, reject) => {
    if (!authUser || !authUser.uid) {
      reject('Authentication user not defined.');
      return;
    }

    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection('users').doc(authUser.uid).get();

      if (!snapshot.exists) {
        const user: User = await createUserData(authUser);

        resolve({...user});
        return;
      }

      const user: UserData = snapshot.data() as UserData;

      const updatedUser: UserData = await updateUserWithAuthData(authUser, user);

      resolve({
        id: authUser.uid,
        data: updatedUser
      });
    } catch (err) {
      reject(err);
    }
  });
};

export const updateUser: UpdateUser = (user: User): Promise<User> => {
  return new Promise<User>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
    user.data.updated_at = now as unknown as Date;

    // We delete username and apiUserId as we do not want to save these information in Firestore
    const data: ApiUserData = {
      ...user.data
    } as ApiUserData;

    delete data.username;
    delete data.apiUserId;

    try {
      await firestore.collection('users').doc(user.id).set(data, {merge: true});

      resolve({...user});
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteUser: DeleteUser = (userId: string): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      await firestore.collection('users').doc(userId).delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const createUserData = (authUser: AuthUser): Promise<User> => {
  return new Promise<User>(async (resolve, reject) => {
    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

      const user: UserData = {
        anonymous: authUser.anonymous,
        newsletter: true,
        created_at: now as unknown as Date,
        updated_at: now as unknown as Date
      };

      if (authUser.name) {
        user.name = authUser.name;
      }

      if (authUser.email) {
        user.email = authUser.email;
        user.newsletter = true;
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
};

const updateUserWithAuthData = (authUser: AuthUser, user: UserData): Promise<UserData> => {
  return new Promise<UserData>(async (resolve, reject) => {
    try {
      if (userNeedUpdate(authUser, user)) {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        if (user.anonymous !== authUser.anonymous) {
          user.anonymous = authUser.anonymous;
        }

        if (!user.name && authUser.name) {
          user.name = authUser.name;
        }

        if (!user.email && authUser.email) {
          user.email = authUser.email;
        }

        if (!user.photo_url && authUser.photo_url) {
          user.photo_url = authUser.photo_url;
        }

        user.updated_at = firebase.firestore.Timestamp.now() as unknown as Date;

        await firestore.collection('users').doc(authUser.uid).set(user, {merge: true});
      }

      resolve(user);
    } catch (err) {
      reject(err);
    }
  });
};

const userNeedUpdate = (authUser: AuthUser, user: UserData): boolean => {
  if (user.anonymous !== authUser.anonymous) {
    return true;
  } else if (!user.name && authUser.name) {
    return true;
  } else if (!user.email && authUser.email) {
    return true;
  } else if (!user.photo_url && authUser.photo_url) {
    return true;
  } else {
    return false;
  }
};
