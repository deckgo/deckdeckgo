import {AuthUser} from '../models/auth/auth.user';
import {User} from '../models/data/user';

export interface InitAuth {
  ({
    config,
    success,
    reset
  }: {
    config: Record<string, string | boolean>;
    success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
    reset: () => Promise<void>;
  }): Promise<void>;
}

export interface SignOut {
  (): Promise<void>;
}

export interface DeleteAuth {
  ({user, config}: {user: User; config: Record<string, string | boolean>}): Promise<void>;
}
