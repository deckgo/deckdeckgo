import {AuthUser} from '../models/auth/auth.user';
import {User} from '../models/data/user';

export interface CreateUser {
  (authUser: AuthUser): Promise<User>;
}

export interface UpdateUser {
  (user: User): Promise<User>;
}
