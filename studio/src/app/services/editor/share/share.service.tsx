import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import deckStore from '../../../stores/deck.store';
import authStore from '../../../stores/auth.store';

export class ShareService {
  private static instance: ShareService;

  static getInstance() {
    if (!ShareService.instance) {
      ShareService.instance = new ShareService();
    }
    return ShareService.instance;
  }

  getPublishedUrl(): Promise<string> {
    return new Promise<string>((resolve) => {
      if (
        deckStore.state.deck &&
        deckStore.state.deck.data &&
        deckStore.state.deck.data.meta &&
        deckStore.state.deck.data.meta.pathname &&
        deckStore.state.deck.data.meta.pathname !== ''
      ) {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
        resolve(config.presentationUrl + deckStore.state.deck.data.meta.pathname);
      } else {
        // Should not happens
        const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
        resolve(deckDeckGoConfig.appUrl);
      }
    });
  }

  getShareText(): Promise<string> {
    return new Promise<string>((resolve) => {
      if (deckStore.state.deck && deckStore.state.deck.data && deckStore.state.deck.data.name && deckStore.state.deck.data.name !== '') {
        if (!authStore.state.anonymous && authStore.state.authUser && authStore.state.authUser.name && authStore.state.authUser.name !== '') {
          resolve(`"${deckStore.state.deck.data.name}" by ${authStore.state.authUser.name} created with DeckDeckGo`);
        } else {
          resolve(`"${deckStore.state.deck.data.name}" created with DeckDeckGo`);
        }
      } else {
        resolve('A presentation created with DeckDeckGo');
      }
    });
  }
}
