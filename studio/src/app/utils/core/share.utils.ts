import {EnvironmentDeckDeckGoConfig} from '../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

import {Deck} from '../../models/data/deck';
import {UserSocial} from '../../models/data/user';

export async function getPublishedUrl(deck: Deck | null): Promise<string> {
  if (deck?.data?.meta?.pathname !== '') {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return config.presentationUrl + deck.data.meta.pathname;
  } else {
    // Should not happens
    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return deckDeckGoConfig.appUrl;
  }
}

export async function getShareText(deck: Deck | null, userName: string | undefined): Promise<string> {
  return getCommonShareText(deck, userName, 'DeckDeckGo');
}

export async function getShareTwitterText(deck: Deck | null, userName: string | undefined, userSocial: UserSocial | undefined): Promise<string> {
  if (!userSocial || userSocial === undefined || !userSocial.twitter || userSocial.twitter === undefined || userSocial.twitter === '') {
    return getCommonShareText(deck, userName, '@deckdeckgo');
  }

  if (deck?.data?.name !== '') {
    return `"${deck.data.name}" by @${userSocial.twitter} created with @deckdeckgo`;
  } else {
    return `A presentation by ${userSocial.twitter} created with @deckdeckgo`;
  }
}

async function getCommonShareText(deck: Deck | null, userName: string | undefined, deckDeckGo: string): Promise<string> {
  if (deck?.data?.name !== '') {
    if (userName && userName !== undefined && userName !== '') {
      return `"${deck.data.name}" by ${userName} created with ${deckDeckGo}`;
    } else {
      return `"${deck.data.name}" created with ${deckDeckGo}`;
    }
  } else {
    return `A presentation created with ${deckDeckGo}`;
  }
}
