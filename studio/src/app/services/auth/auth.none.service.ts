import {AuthService} from './auth.service';

export class AuthNoneService extends AuthService {
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
