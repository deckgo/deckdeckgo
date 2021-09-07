import {AuthProvider} from './auth.service';

export class AuthOfflineProvider extends AuthProvider {
  // @Override
  async init() {
    // Do nothing
  }

  // @Override
  async signIn() {
    // Do nothing
  }

  // @Override
  async signOut() {
    // Do nothing
  }
}
