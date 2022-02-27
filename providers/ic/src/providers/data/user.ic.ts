import {log, UpdateUser, User, UserData} from '@deckdeckgo/editor';
import {setData} from '../../utils/data.utils';

export const updateUser: UpdateUser = async (user: User): Promise<User> => {
  log({msg: '[update][start] user'});
  const t0 = performance.now();

  const {data, id} = user;

  const updatedUser: User = await setData<User, UserData>({key: `/user`, id, data});

  const t1 = performance.now();
  log({msg: '[update][done] user', duration: t1 - t0});

  return updatedUser;
};
