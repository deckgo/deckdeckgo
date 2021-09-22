import {AuthUser, InitAuth, SignOut, SignIn, User, DeleteAuth} from '@deckdeckgo/editor';

export const initAuth: InitAuth = async (_param: {
  config: Record<string, string | boolean>;
  success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
  reset: () => Promise<void>;
}) => {
  // TODO
};

export const signOut: SignOut = async () => {
  // TODO
};

export const signIn: SignIn = async () => {
  // TODO
};

export const deleteAuth: DeleteAuth = async (_param: {user: User; config}) => {
  // TODO
};
