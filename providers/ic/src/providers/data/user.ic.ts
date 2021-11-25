import {UpdateUser, User, UserData} from '@deckdeckgo/editor';

import {setData} from '../../utils/data.utils';

export const updateUser: UpdateUser = async (user: User): Promise<User> => {
  console.log('User IC about to SET', updateUser);
  const t0 = performance.now();

  const {data, id} = user;

  const updatedUser: User = await setData<User, UserData>({key: `/user`, id, data});

  const t1 = performance.now();
  console.log('User IC SET done', t1 - t0);

  return updatedUser;
};
