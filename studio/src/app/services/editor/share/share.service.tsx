import {take} from 'rxjs/operators';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import store from '../../../stores/deck.store';

import {AuthUser} from '../../../models/auth/auth.user';

import {AuthService} from '../../auth/auth.service';

export class ShareService {
  private static instance: ShareService;

  private authService: AuthService;

  private constructor() {
    this.authService = AuthService.getInstance();
  }

  static getInstance() {
    if (!ShareService.instance) {
      ShareService.instance = new ShareService();
    }
    return ShareService.instance;
  }

  getPublishedUrl(): Promise<string> {
    return new Promise<string>((resolve) => {
      if (
        store.state.deck &&
        store.state.deck.data &&
        store.state.deck.data.meta &&
        store.state.deck.data.meta.pathname &&
        store.state.deck.data.meta.pathname !== ''
      ) {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
        resolve(config.presentationUrl + store.state.deck.data.meta.pathname);
      } else {
        // Should not happens
        const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
        resolve(deckDeckGoConfig.appUrl);
      }
    });
  }

  getShareText(): Promise<string> {
    return new Promise<string>((resolve) => {
      if (store.state.deck && store.state.deck.data && store.state.deck.data.name && store.state.deck.data.name !== '') {
        this.authService
          .watch()
          .pipe(take(1))
          .subscribe(async (authUser: AuthUser) => {
            if (authUser && !authUser.anonymous && authUser.name && authUser.name !== '') {
              resolve(`"${store.state.deck.data.name}" by ${authUser.name} created with DeckDeckGo`);
            } else {
              resolve(`"${store.state.deck.data.name}" created with DeckDeckGo`);
            }
          });
      } else {
        resolve('A presentation created with DeckDeckGo');
      }
    });
  }
}
