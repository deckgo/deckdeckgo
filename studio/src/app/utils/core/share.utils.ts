import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

import i18n from '../../stores/i18n.store';

import {Deck} from '../../models/data/deck';
import {UserSocial} from '../../models/data/user';

import {i18nFormat} from './i18n.utils';

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
    return i18nFormat(i18n.state.share.a_presentation_by, [
      {regex: /\{0\}/g, value: `"${deck.data.name}"`},
      {regex: /\{1\}/g, value: `@${userSocial.twitter}`},
      {regex: /\{2\}/g, value: `@deckdeckgo`},
    ]);
  } else {
    return i18nFormat(i18n.state.share.a_presentation_by, [
      {regex: /\{0\}/g, value: `A presentation`},
      {regex: /\{1\}/g, value: `@${userSocial.twitter}`},
      {regex: /\{2\}/g, value: `@deckdeckgo`},
    ]);
  }
}

async function getCommonShareText(deck: Deck | null, userName: string | undefined, deckDeckGo: string): Promise<string> {
  if (deck?.data?.name !== '') {
    if (userName && userName !== undefined && userName !== '') {
      return i18nFormat(i18n.state.share.a_presentation_by, [
        {regex: /\{0\}/g, value: `"${deck.data.name}"`},
        {regex: /\{1\}/g, value: `${userName}`},
        {regex: /\{2\}/g, value: `${deckDeckGo}`},
      ]);
    } else {
      return i18nFormat(i18n.state.share.a_presentation, [
        {regex: /\{0\}/g, value: `"${deck.data.name}"`},
        {regex: /\{1\}/g, value: `${deckDeckGo}`},
      ]);
    }
  } else {
    return i18nFormat(i18n.state.share.a_presentation, [
      {regex: /\{0\}/g, value: `A presentation`},
      {regex: /\{1\}/g, value: `${deckDeckGo}`},
    ]);
  }
}
