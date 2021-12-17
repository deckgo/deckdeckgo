import i18n from '../../stores/i18n.store';
import shareStore, {ShareData} from '../../stores/share.store';
import editorStore from '../../stores/editor.store';
import userStore from '../../stores/user.store';

import {i18nFormat} from './i18n.utils';

export const share = () => {
  const title: string | undefined =
    editorStore.state.doc !== null
      ? editorStore.state.doc.data.meta?.title ?? i18n.state.share.a_document
      : editorStore.state.deck.data.meta?.title ?? i18n.state.share.a_presentation;

  shareStore.state.share = {
    title,
    userName: userStore.state.name,
    userSocial: userStore.state.social
  };
};

export const getShareText = (): string => getCommonShareText();

export const getShareTwitterText = (): string => {
  const {title, userSocial}: ShareData = shareStore.state.share;

  if (!userSocial || userSocial === undefined || !userSocial.twitter || userSocial.twitter === undefined || userSocial.twitter === '') {
    return getCommonShareText();
  }

  return i18nFormat(i18n.state.share.content_by, [
    {placeholder: '{0}', value: `${title}`},
    {placeholder: '{1}', value: `@${userSocial.twitter}`},
    {placeholder: '{2}', value: `@deckdeckgo`}
  ]);
};

// i18n.state.share.a_presentation
const getCommonShareText = (): string => {
  const {userName, title}: ShareData = shareStore.state.share;
  const deckDeckGo: string = 'DeckDeckGo';

  if (userName && userName !== undefined && userName !== '') {
    return i18nFormat(i18n.state.share.content_by, [
      {placeholder: '{0}', value: `${title}`},
      {placeholder: '{1}', value: `${userName}`},
      {placeholder: '{2}', value: `${deckDeckGo}`}
    ]);
  }

  return i18nFormat(i18n.state.share.content_no_author, [
    {placeholder: '{0}', value: `${title}`},
    {placeholder: '{1}', value: `${deckDeckGo}`}
  ]);
};
