export abstract class AuthProvider {
  abstract init(): Promise<void>;

  abstract signIn(): Promise<void>;

  abstract signOut(): Promise<void>;
}
