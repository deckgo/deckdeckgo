import {EnvironmentDeckDeckGoConfig} from '../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

import {Deck} from '../../models/data/deck';

export async function getPublishedUrl(deck: Deck | null): Promise<string> {
  if (deck && deck.data && deck.data.meta && deck.data.meta.pathname && deck.data.meta.pathname !== '') {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return config.presentationUrl + deck.data.meta.pathname;
  } else {
    // Should not happens
    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return deckDeckGoConfig.appUrl;
  }
}

export async function getShareText(deck: Deck | null, userName: string | undefined): Promise<string> {
  if (deck && deck.data && deck.data.name && deck.data.name !== '') {
    if (userName && userName !== undefined && userName !== '') {
      return `"${deck.data.name}" by ${userName} created with DeckDeckGo`;
    } else {
      return `"${deck.data.name}" created with DeckDeckGo`;
    }
  } else {
    return 'A presentation created with DeckDeckGo';
  }
}
