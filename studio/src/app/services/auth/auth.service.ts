export abstract class AuthService {
  abstract init(): Promise<void>;

  abstract signIn(): Promise<void>;

  abstract signOut(): Promise<void>;
}
