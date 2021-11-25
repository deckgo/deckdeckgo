import {Deck, UserSocial} from '@deckdeckgo/editor';

import i18n from '../../stores/i18n.store';

import {i18nFormat} from './i18n.utils';

export const getShareText = (deck: Deck | null, userName: string | undefined): string => {
  return getCommonShareText(deck, userName, 'DeckDeckGo');
};

export const getShareTwitterText = (deck: Deck | null, userName: string | undefined, userSocial: UserSocial | undefined): string => {
  if (!userSocial || userSocial === undefined || !userSocial.twitter || userSocial.twitter === undefined || userSocial.twitter === '') {
    return getCommonShareText(deck, userName, '@deckdeckgo');
  }

  if (deck?.data?.name !== '') {
    return i18nFormat(i18n.state.share.a_presentation_by, [
      {placeholder: '{0}', value: `"${deck.data.name}"`},
      {placeholder: '{1}', value: `@${userSocial.twitter}`},
      {placeholder: '{2}', value: `@deckdeckgo`}
    ]);
  } else {
    return i18nFormat(i18n.state.share.a_presentation_by, [
      {placeholder: '{0}', value: i18n.state.share.a_presentation},
      {placeholder: '{1}', value: `@${userSocial.twitter}`},
      {placeholder: '{2}', value: `@deckdeckgo`}
    ]);
  }
};

const getCommonShareText = (deck: Deck | null, userName: string | undefined, deckDeckGo: string): string => {
  if (deck?.data?.name !== '') {
    if (userName && userName !== undefined && userName !== '') {
      return i18nFormat(i18n.state.share.a_presentation_by, [
        {placeholder: '{0}', value: `"${deck.data.name}"`},
        {placeholder: '{1}', value: `${userName}`},
        {placeholder: '{2}', value: `${deckDeckGo}`}
      ]);
    } else {
      return i18nFormat(i18n.state.share.a_presentation_no_author, [
        {placeholder: '{0}', value: `"${deck.data.name}"`},
        {placeholder: '{1}', value: `${deckDeckGo}`}
      ]);
    }
  } else {
    return i18nFormat(i18n.state.share.a_presentation_no_author, [
      {placeholder: '{0}', value: i18n.state.share.a_presentation},
      {placeholder: '{1}', value: `${deckDeckGo}`}
    ]);
  }
};
